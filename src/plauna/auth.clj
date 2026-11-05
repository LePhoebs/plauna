(ns plauna.auth
  "Web UI authentication: login-name-and-password protection plus optional mTLS authentication
   through a trusted reverse proxy.

   The active password is established at startup by initialize! with the precedence:
   PLAUNA_PASSWORD environment variable > previously stored password > a freshly generated one
   (printed to the log once). The hash is persisted in the preferences table and cached in memory.

   mTLS authentication can be configured in the administration UI or through environment variables.
   NGINX must verify the client certificate and forward its escaped PEM value. A separate shared
   secret authenticates NGINX to Plauna so clients cannot gain access by forging the headers."
  (:require [clojure.string :as str]
            [plauna.database :as db]
            [plauna.settings :as settings]
            [taoensso.telemere :as t])
  (:import (javax.crypto SecretKeyFactory)
           (javax.crypto.spec PBEKeySpec)
           (java.io ByteArrayInputStream)
           (java.net URLDecoder)
           (java.nio.charset StandardCharsets)
           (java.security SecureRandom MessageDigest)
           (java.security.cert CertificateFactory X509Certificate)
           (java.util Base64 HexFormat)))

(set! *warn-on-reflection* true)

(def ^:private iterations 210000)
(def ^:private key-bits 256)
(def ^:private salt-bytes 16)

(defonce ^:private ^SecureRandom rng (SecureRandom.))

;; Runtime cache of the stored password hash (a "pbkdf2$iterations$salt$hash" string).
(defonce password-hash (atom nil))

;; Active runtime mTLS configuration. UI-managed secrets are persisted in settings.json but never in
;; the database, log output, rendered HTML or browser session.
(defonce ^:private mtls-auth-config (atom nil))

(defn ^:dynamic system-env [key] (System/getenv key))

(defn- b64 ^String [^bytes bs] (.encodeToString (Base64/getEncoder) bs))
(defn- unb64 ^bytes [^String s] (.decode (Base64/getDecoder) s))

(defn- pbkdf2 ^bytes [^String password ^bytes salt ^long iters ^long bits]
  (let [spec (PBEKeySpec. (.toCharArray password) salt iters bits)
        skf (SecretKeyFactory/getInstance "PBKDF2WithHmacSHA256")]
    (.getEncoded (.generateSecret skf spec))))

(defn hash-password
  "Hash a plaintext password with a fresh random salt. Returns 'pbkdf2$<iterations>$<salt>$<hash>'."
  [^String plaintext]
  (let [salt (byte-array salt-bytes)]
    (.nextBytes rng salt)
    (str "pbkdf2$" iterations "$" (b64 salt) "$" (b64 (pbkdf2 plaintext salt iterations key-bits)))))

(defn verify-password
  "Constant-time check of a plaintext password against a stored 'pbkdf2$...' hash."
  [^String plaintext ^String stored]
  (try
    (let [[algo iters salt-b64 hash-b64] (str/split stored #"\$")]
      (and (= algo "pbkdf2")
           (some? plaintext)
           (let [expected (unb64 hash-b64)
                 actual (pbkdf2 plaintext (unb64 salt-b64) (Long/parseLong iters) (* 8 (alength expected)))]
             (MessageDigest/isEqual expected actual))))
    (catch Exception _ false)))

(defn generate-password
  "Generate a strong random password (URL-safe, ~32 characters)."
  []
  (let [bs (byte-array 24)]
    (.nextBytes rng bs)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs)))

(defn set-password!
  "Hash and persist a new web UI password, updating the in-memory cache."
  [^String plaintext]
  (let [h (hash-password plaintext)]
    (reset! password-hash h)
    (db/update-preference :web-password-hash h)))

(defn verify-web-password?
  "True if plaintext matches the active web UI password."
  [plaintext]
  (let [h @password-hash]
    (and (some? h) (verify-password plaintext h))))

(defn- constant-time-string= [expected actual]
  (and (string? expected)
       (string? actual)
       (MessageDigest/isEqual (.getBytes ^String expected StandardCharsets/UTF_8)
                              (.getBytes ^String actual StandardCharsets/UTF_8))))

(defn- normalized-login-name [value]
  (let [login-name (some-> value str str/trim)]
    (when (str/blank? login-name)
      (throw (ex-info "The login name must not be empty." {})))
    (when (> (count login-name) 128)
      (throw (ex-info "The login name must not exceed 128 characters." {})))
    (when (re-find #"\p{Cntrl}" login-name)
      (throw (ex-info "The login name must not contain control characters." {})))
    login-name))

(defn web-login-name
  "Return the configured web UI login name. Existing installations default to root. A malformed
   value in settings.json fails safely back to that default rather than locking out the UI."
  []
  (try
    (normalized-login-name (or (settings/fetch-setting :web-login-name) "root"))
    (catch clojure.lang.ExceptionInfo _ "root")))

(defn set-login-name!
  "Validate and persist the web UI login name. Returns its canonical trimmed value."
  [login-name]
  (let [canonical (normalized-login-name login-name)]
    (settings/update-settings! {:web-login-name canonical})
    canonical))

(defn verify-web-credentials?
  "True only when both the configured login name and active web UI password match. Both checks always
   run: short-circuiting on the login name would answer a wrong name in microseconds and a right one
   only after the PBKDF2 work, telling an attacker which names are worth trying."
  [login-name plaintext]
  (let [name-ok? (constant-time-string= (web-login-name) login-name)
        password-ok? (verify-web-password? plaintext)]
    (and name-ok? password-ok?)))

;; ── Login throttling ────────────────────────────────────────────────────────────

(def login-failure-window-millis
  "Failures older than this no longer slow down login attempts."
  (* 15 60 1000))

(def max-login-delay-millis 5000)

(defonce ^:private login-failures (atom {:count 0 :last-failure 0}))

(defn- failure-after
  "The throttle state after one more failure at now."
  [{:keys [count last-failure]} now]
  (let [recent? (< (- (long now) (long last-failure)) login-failure-window-millis)]
    {:count (if recent? (inc (long count)) 1) :last-failure now}))

(defn- delay-millis [{:keys [count last-failure]} now]
  (if (or (zero? (long count))
          (>= (- (long now) (long last-failure)) login-failure-window-millis))
    0
    (min max-login-delay-millis (* 250 (long count) (long count)))))

(defn- wait-remaining-millis [{:keys [last-failure] :as state} now]
  (let [delay (delay-millis state now)]
    (if (zero? (long delay))
      0
      (max 0 (- (+ (long last-failure) (long delay)) (long now))))))

(defn record-login-failure! []
  (swap! login-failures failure-after (System/currentTimeMillis)))

(defn record-login-success! []
  (reset! login-failures {:count 0 :last-failure 0}))

(defn login-delay-millis
  "The pause the recent failures impose on the next login attempt: nothing after a clean history, then
   growing with each failure in the window (250 ms, 1 s, 2.25 s ... capped at 5 s). This is a global
   brake, not a lockout: the administrator can always log in, just not quickly after a burst of wrong
   guesses, and a guessing script is held to a few attempts per minute."
  []
  (delay-millis @login-failures (System/currentTimeMillis)))

(defn login-wait-remaining-millis
  "How much of the pause after the last failure is still ahead of us: an attempt arriving earlier is
   refused outright (no password check, no thread parked in a sleep), one arriving later is checked."
  []
  (wait-remaining-millis @login-failures (System/currentTimeMillis)))

(defn claim-login-attempt!
  "Atomically decide whether a login attempt may be checked now. Returns 0 when it may - the attempt is
   then already counted as a failure, so attempts arriving in parallel see its pause and are refused;
   record-login-success! clears it again - or the milliseconds still to wait."
  []
  (let [now (System/currentTimeMillis)
        [old new] (swap-vals! login-failures
                              (fn [state] (if (pos? (long (wait-remaining-millis state now)))
                                            state
                                            (failure-after state now))))]
    (if (identical? old new) (wait-remaining-millis old now) 0)))

(defn password-from-env-var?
  "True if the password is being supplied via the PLAUNA_PASSWORD environment variable."
  []
  (not (str/blank? (system-env "PLAUNA_PASSWORD"))))

(defn- normalize-sha256-fingerprint [fingerprint]
  (let [normalized (some-> fingerprint
                           str/trim
                           str/lower-case
                           (str/replace ":" ""))]
    (when (and normalized (re-matches #"[0-9a-f]{64}" normalized))
      normalized)))

(defn mtls-config-from-env
  "Build the mTLS proxy configuration from an environment map/function. Returns nil when no
   certificate allowlist is configured and throws for incomplete or malformed opt-in settings."
  [env]
  (let [lookup       #(if (map? env) (get env %) (env %))
        raw-certs    (lookup "PLAUNA_MTLS_TRUSTED_CERT_SHA256")
        proxy-secret (lookup "PLAUNA_MTLS_PROXY_SECRET")]
    (when-not (str/blank? raw-certs)
      (when (< (count (or proxy-secret "")) 32)
        (throw (ex-info "PLAUNA_MTLS_PROXY_SECRET must contain at least 32 characters when mTLS authentication is enabled."
                        {})))
      (let [provided     (remove str/blank? (str/split raw-certs #"[,;\s]+"))
            fingerprints (map normalize-sha256-fingerprint provided)]
        (when (or (empty? provided) (some nil? fingerprints))
          (throw (ex-info "PLAUNA_MTLS_TRUSTED_CERT_SHA256 must contain only full SHA-256 certificate fingerprints."
                          {})))
        {:proxy-secret proxy-secret
         :fingerprints (set fingerprints)}))))

(defn mtls-environment-managed?
  "True when the certificate allowlist environment variable selects the environment as the complete
   mTLS configuration source. In that case both mTLS values are read-only in the web UI."
  []
  (not (str/blank? (system-env "PLAUNA_MTLS_TRUSTED_CERT_SHA256"))))

(defn- stored-mtls-values []
  {"PLAUNA_MTLS_TRUSTED_CERT_SHA256" (settings/fetch-setting :mtls-trusted-cert-sha256)
   "PLAUNA_MTLS_PROXY_SECRET"        (settings/fetch-setting :mtls-proxy-secret)})

(defn- environment-mtls-values []
  {"PLAUNA_MTLS_TRUSTED_CERT_SHA256" (system-env "PLAUNA_MTLS_TRUSTED_CERT_SHA256")
   "PLAUNA_MTLS_PROXY_SECRET"        (system-env "PLAUNA_MTLS_PROXY_SECRET")})

(defn- effective-mtls-values []
  (if (mtls-environment-managed?)
    (environment-mtls-values)
    (stored-mtls-values)))

(defn- activate-mtls-config! [config]
  (reset! mtls-auth-config config)
  (when config
    (t/log! :info ["mTLS proxy authentication enabled for"
                   (count (:fingerprints config))
                   "certificate fingerprint(s)."])))

(defn initialize-mtls!
  "Load optional mTLS proxy authentication from the environment or settings.json. Environment
   configuration has priority. Invalid opt-in configuration fails closed: password authentication
   remains available, but forwarded certificates are not accepted."
  []
  (try
    (activate-mtls-config! (mtls-config-from-env (effective-mtls-values)))
    (catch Exception e
      (reset! mtls-auth-config nil)
      (t/log! {:level :error :error e}
              "mTLS proxy authentication is disabled because its configuration is invalid."))))

(defn mtls-admin-state
  "Return the non-secret mTLS state suitable for rendering in the administration UI."
  []
  (let [environment-managed (mtls-environment-managed?)
        values              (effective-mtls-values)
        raw-fingerprints    (or (get values "PLAUNA_MTLS_TRUSTED_CERT_SHA256") "")
        proxy-secret        (get values "PLAUNA_MTLS_PROXY_SECRET")
        canonical           (try
                              (some->> (mtls-config-from-env values)
                                       :fingerprints
                                       sort
                                       (str/join "\n"))
                              (catch Exception _ nil))]
    {:environment-managed environment-managed
     :enabled             (some? @mtls-auth-config)
     :secret-configured   (not (str/blank? proxy-secret))
     :trusted-cert-sha256 (or canonical raw-fingerprints)}))

(defn save-mtls-settings!
  "Validate and atomically save mTLS values submitted by the administration UI. A blank secret keeps
   the stored one; :clear-proxy-secret explicitly removes it. The current admin password is required
   for every change because editing the allowlist grants future access. Returns non-secret UI state."
  [{:keys [trusted-cert-sha256 proxy-secret clear-proxy-secret current-password]}]
  (when-not (verify-web-password? current-password)
    (throw (ex-info "Current admin password is incorrect." {})))
  (when (mtls-environment-managed?)
    (throw (ex-info "mTLS authentication is managed by environment variables and cannot be changed here."
                    {})))
  (let [existing-secret (settings/fetch-setting :mtls-proxy-secret)
        clear-secret?   (or (true? clear-proxy-secret) (= "true" clear-proxy-secret))
        new-secret      (cond
                          clear-secret? ""
                          (str/blank? proxy-secret) (or existing-secret "")
                          :else proxy-secret)
        raw-certs       (or trusted-cert-sha256 "")]
    (when (and (str/blank? raw-certs)
               (not (str/blank? proxy-secret))
               (< (count proxy-secret) 32))
      (throw (ex-info "The proxy secret must contain at least 32 characters." {})))
    (let [config          (mtls-config-from-env
                           {"PLAUNA_MTLS_TRUSTED_CERT_SHA256" raw-certs
                            "PLAUNA_MTLS_PROXY_SECRET" new-secret})
          canonical-certs (if config
                            (str/join "\n" (sort (:fingerprints config)))
                            "")]
      (settings/update-settings! {:mtls-trusted-cert-sha256 canonical-certs
                                  :mtls-proxy-secret new-secret})
      (activate-mtls-config! config)
      (mtls-admin-state))))

(defn client-certificate-sha256
  "Decode an NGINX $ssl_client_escaped_cert header and return the certificate's canonical SHA-256
   fingerprint, or nil when the header does not contain one valid X.509 certificate."
  [escaped-pem]
  (when-not (str/blank? escaped-pem)
    (try
      (let [pem     (URLDecoder/decode ^String escaped-pem StandardCharsets/UTF_8)
            factory (CertificateFactory/getInstance "X.509")]
        (with-open [input (ByteArrayInputStream. (.getBytes ^String pem StandardCharsets/UTF_8))]
          (let [certificate ^X509Certificate (.generateCertificate factory input)
                digest      (.digest (MessageDigest/getInstance "SHA-256") (.getEncoded certificate))]
            (.formatHex (HexFormat/of) digest))))
      (catch Exception _ nil))))

(defn verified-mtls-client-fingerprint-with-secret
  "Return the forwarded certificate fingerprint only when the proxy secret matches and NGINX says
   certificate validation succeeded. This intentionally does not consult the allowlist."
  [proxy-secret request]
  (let [headers (:headers request)]
    (when (and (not (str/blank? proxy-secret))
               (constant-time-string= proxy-secret (get headers "x-plauna-proxy-secret"))
               (= "SUCCESS" (get headers "x-plauna-client-verify")))
      (client-certificate-sha256 (get headers "x-plauna-client-cert")))))

(defn verified-mtls-client-fingerprint
  "Return the SHA-256 fingerprint of a CA-verified client certificate forwarded by the configured
   trusted proxy, even when that certificate is not in Plauna's allowlist yet."
  [request]
  (let [proxy-secret (get (effective-mtls-values) "PLAUNA_MTLS_PROXY_SECRET")]
    (when (<= 32 (count (or proxy-secret "")))
      (verified-mtls-client-fingerprint-with-secret proxy-secret request))))

(defn mtls-admin-certificate-state
  "Return safe administration-page data for a client certificate verified by the trusted proxy.
   The fingerprint is derived from the request and is never accepted from browser form data."
  [request]
  (when-let [fingerprint (verified-mtls-client-fingerprint request)]
    (let [trusted             (contains? (:fingerprints @mtls-auth-config) fingerprint)
          environment-managed (mtls-environment-managed?)]
      {:fingerprint         fingerprint
       :trusted             trusted
       :can-add             (and (not trusted) (not environment-managed))
       :environment-managed environment-managed})))

(defn save-mtls-settings-from-request!
  "Save mTLS settings submitted from the administration page. When requested, append the current
   proxy-verified certificate before the normal validation and atomic save. The fingerprint is
   derived exclusively from trusted proxy headers, never from a form parameter."
  [request]
  (let [params       (:params request)
        add-current? (= "true" (:add-current-certificate params))]
    (if-not add-current?
      (save-mtls-settings! params)
      (let [fingerprint (verified-mtls-client-fingerprint request)]
        (when-not fingerprint
          (throw (ex-info "No successfully verified client certificate was supplied by the trusted proxy."
                          {})))
        (save-mtls-settings!
         (assoc params :trusted-cert-sha256
                (str (or (:trusted-cert-sha256 params) "") "\n" fingerprint)))))))

(defn mtls-request-authorized-with-config?
  "True when a request carries the three headers supplied by the trusted NGINX configuration and
   the verified client certificate is in the configured SHA-256 allowlist."
  [config request]
  (boolean
   (and config
        (when-let [fingerprint (verified-mtls-client-fingerprint-with-secret
                                (:proxy-secret config) request)]
          (contains? (:fingerprints config) fingerprint)))))

(defn mtls-request-authorized?
  "True when the request was authenticated with an explicitly allowed mTLS client certificate."
  [request]
  (mtls-request-authorized-with-config? @mtls-auth-config request))

(defn initialize!
  "Establish the optional mTLS proxy authentication and web UI password at startup. Password precedence:
   1. PLAUNA_PASSWORD env var (authoritative on every boot),
   2. a previously stored password,
   3. a freshly generated password (logged once)."
  []
  (initialize-mtls!)
  (let [env-pw (system-env "PLAUNA_PASSWORD")
        stored (db/fetch-preference :web-password-hash)]
    (cond
      (not (str/blank? env-pw))
      (do (set-password! env-pw)
          (t/log! :info "Web UI password set from the PLAUNA_PASSWORD environment variable."))

      (some? stored)
      (reset! password-hash stored)

      :else
      (let [pw (generate-password)]
        (set-password! pw)
        (t/log! :warn (str "================================================================\n"
                           "No web UI password configured. A temporary password was generated:\n\n"
                           "    " pw "\n\n"
                           "Log in as root and change it under Administration > Login Settings,\n"
                           "or set the PLAUNA_PASSWORD environment variable.\n"
                           "================================================================"))))))
