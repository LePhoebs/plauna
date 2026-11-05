(ns plauna.auth-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [plauna.auth :as auth]
            [plauna.settings :as settings]))

(deftest hash-and-verify-roundtrip
  (let [hash (auth/hash-password "correct horse battery staple")]
    (is (str/starts-with? hash "pbkdf2$") "Hash is stored in the pbkdf2$... format")
    (is (true? (auth/verify-password "correct horse battery staple" hash)) "The correct password verifies")
    (is (false? (auth/verify-password "wrong password" hash)) "An incorrect password does not verify"))
  "hash-password and verify-password round-trip correctly")

(deftest each-hash-uses-a-fresh-salt
  (let [a (auth/hash-password "same-password")
        b (auth/hash-password "same-password")]
    (is (not= a b) "Hashing the same password twice yields different output (random salt)")
    (is (true? (auth/verify-password "same-password" a)))
    (is (true? (auth/verify-password "same-password" b))))
  "A random salt is used per hash")

(deftest verify-is-safe-on-garbage-input
  (is (false? (auth/verify-password "x" "not-a-valid-hash")))
  (is (false? (auth/verify-password "x" "")))
  (is (false? (auth/verify-password nil (auth/hash-password "y"))))
  "verify-password returns false rather than throwing on malformed input")

(deftest generated-passwords-are-strong-and-unique
  (let [a (auth/generate-password)
        b (auth/generate-password)]
    (is (>= (count a) 20) "Generated password is long")
    (is (not= a b) "Generated passwords are unique"))
  "generate-password produces strong, unique passwords")

(deftest verify-web-password-uses-the-cached-hash
  (with-redefs [auth/password-hash (atom (auth/hash-password "the-secret"))]
    (is (true? (auth/verify-web-password? "the-secret")))
    (is (false? (auth/verify-web-password? "nope")))
    (is (false? (auth/verify-web-password? nil))))
  "verify-web-password? checks the plaintext against the in-memory hash")

(deftest web-login-name-defaults-to-root-and-is-configurable
  (with-redefs [settings/fetch-setting (constantly nil)]
    (is (= "root" (auth/web-login-name))))
  (with-redefs [settings/fetch-setting (fn [key] (when (= key :web-login-name) "alice"))]
    (is (= "alice" (auth/web-login-name))))
  (let [saved (atom nil)]
    (with-redefs [settings/update-settings! #(reset! saved %)]
      (is (= "alice" (auth/set-login-name! "  alice  ")))
      (is (= {:web-login-name "alice"} @saved))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must not be empty"
                            (auth/set-login-name! "  ")))))
  "Existing installations use root until an administrator selects another login name")

(deftest web-login-requires-both-name-and-password
  (with-redefs [settings/fetch-setting (fn [key] (when (= key :web-login-name) "alice"))
                auth/verify-web-password? #(= "the-secret" %)]
    (is (true? (auth/verify-web-credentials? "alice" "the-secret")))
    (is (false? (auth/verify-web-credentials? "root" "the-secret")))
    (is (false? (auth/verify-web-credentials? "alice" "wrong"))))
  "A valid password alone no longer authenticates when the login name differs")

(deftest mtls-configuration-is-opt-in-and-normalizes-fingerprints
  (let [fingerprint (apply str (repeat 32 "ab"))
        with-colons (str/join ":" (map #(apply str %) (partition-all 2 fingerprint)))
        config (auth/mtls-config-from-env
                {"PLAUNA_MTLS_TRUSTED_CERT_SHA256" (str with-colons ", " (str/upper-case fingerprint))
                 "PLAUNA_MTLS_PROXY_SECRET" (apply str (repeat 32 "s"))})]
    (is (nil? (auth/mtls-config-from-env {})) "No allowlist leaves mTLS authentication disabled")
    (is (= #{fingerprint} (:fingerprints config)) "Colon-delimited and uppercase forms normalize")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"at least 32"
                          (auth/mtls-config-from-env
                           {"PLAUNA_MTLS_TRUSTED_CERT_SHA256" fingerprint
                            "PLAUNA_MTLS_PROXY_SECRET" "too-short"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"only full SHA-256"
                          (auth/mtls-config-from-env
                           {"PLAUNA_MTLS_TRUSTED_CERT_SHA256" "not-a-fingerprint"
                            "PLAUNA_MTLS_PROXY_SECRET" (apply str (repeat 32 "s"))}))))
  "mTLS cannot be enabled accidentally or with an incomplete security configuration")

(deftest nginx-escaped-client-certificate-is-decoded-and-fingerprinted
  (let [pem "-----BEGIN CERTIFICATE-----
MIICEjCCAXugAwIBAgIUax4S8bdFcu8CFsqSW3efMJzaFR8wDQYJKoZIhvcNAQEL
BQAwGzEZMBcGA1UEAwwQUGxhdW5hLW1UTFMtdGVzdDAeFw0yNjA4MDkxMDUzNTVa
Fw0zNjA4MDYxMDUzNTVaMBsxGTAXBgNVBAMMEFBsYXVuYS1tVExTLXRlc3QwgZ8w
DQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBALl6VTumaTgVxbT+5QHEpEeg9amXlMiK
sI/YM6XKjOLxp21PTRQaMvZakKEv6m68LzD4Vhtpz6QTjsnS3rddFddfh12DDo2E
uLY26nccOwUT467M83PiBZXLWrqKjB9efyNNKL2tmxv8hXt+EmBLprx1szxZxN1A
ObqXHm8pdgrbAgMBAAGjUzBRMB0GA1UdDgQWBBSIQbB/5fwHLF68lzyHOaIu+ZBP
JjAfBgNVHSMEGDAWgBSIQbB/5fwHLF68lzyHOaIu+ZBPJjAPBgNVHRMBAf8EBTAD
AQH/MA0GCSqGSIb3DQEBCwUAA4GBAHzi6YvjzDO2a2qkQMqau+ap8F14XK/WoTuG
KA8WPmOp/2rD1vdWluGZUgbQ9utfb29jq/BqJQB0KSaH/qXSdvZPmxil2jmC92Pt
bzuLfe5C33mbwSNMdxoMu/9snkZOnkLj4oHIyDrEWQ7LkAVv+Lqx0RRp0EU1AOek
3mQ1d9hv
-----END CERTIFICATE-----"
        escaped (java.net.URLEncoder/encode pem java.nio.charset.StandardCharsets/UTF_8)]
    (is (= "c1c21201ecf8376ee554e44382b9d0abbc342603f79f5340e290a319d83b7b41"
           (auth/client-certificate-sha256 escaped)))
    (is (nil? (auth/client-certificate-sha256 "not-a-certificate"))))
  "Plauna derives SHA-256 from the certificate itself instead of trusting a forwarded identity")

(deftest mtls-authorization-requires-proxy-secret-verification-and-allowlisted-certificate
  (let [fingerprint (apply str (repeat 32 "ab"))
        secret      (apply str (repeat 32 "s"))
        config      {:proxy-secret secret :fingerprints #{fingerprint}}
        request     {:headers {"x-plauna-proxy-secret" secret
                               "x-plauna-client-verify" "SUCCESS"
                               "x-plauna-client-cert" "escaped-certificate"}}]
    (with-redefs [auth/client-certificate-sha256 (constantly fingerprint)]
      (is (true? (auth/mtls-request-authorized-with-config? config request)))
      (is (false? (auth/mtls-request-authorized-with-config?
                   config (assoc-in request [:headers "x-plauna-proxy-secret"] "forged"))))
      (is (false? (auth/mtls-request-authorized-with-config?
                   config (assoc-in request [:headers "x-plauna-client-verify"] "FAILED"))))
      (is (false? (auth/mtls-request-authorized-with-config?
                   (assoc config :fingerprints #{"different"}) request)))))
  "All proxy assertions are required before passwordless access is granted")

(deftest verified-certificate-state-is-available-only-in-mtls-administration
  (let [fingerprint (apply str (repeat 32 "ef"))]
    (with-redefs [auth/system-env (constantly nil)
                  auth/verified-mtls-client-fingerprint (constantly fingerprint)]
      (is (= {:fingerprint fingerprint :trusted false :can-add true :environment-managed false}
             (auth/mtls-admin-certificate-state {}))))
    (with-redefs [auth/system-env #(get {"PLAUNA_MTLS_TRUSTED_CERT_SHA256" (apply str (repeat 32 "ab"))} %)
                  auth/verified-mtls-client-fingerprint (constantly fingerprint)]
      (is (= {:fingerprint fingerprint :trusted false :can-add false :environment-managed true}
             (auth/mtls-admin-certificate-state {})))))
  "A verified certificate can be enrolled from settings in UI mode and is informational in environment mode")

(deftest ui-mtls-settings-are-validated-saved-atomically-and-activated
  (let [fingerprint (apply str (repeat 32 "ab"))
        secret      (apply str (repeat 32 "s"))
        stored      (atom {:mtls-proxy-secret ""})]
    (with-redefs [auth/system-env (constantly nil)
                  auth/verify-web-password? #(= "admin-password" %)
                  settings/fetch-setting #(get @stored %)
                  settings/update-settings! #(swap! stored merge %)]
      (let [state (auth/save-mtls-settings! {:trusted-cert-sha256 (str/upper-case fingerprint)
                                             :proxy-secret secret
                                             :current-password "admin-password"})]
        (is (= fingerprint (:mtls-trusted-cert-sha256 @stored)))
        (is (= secret (:mtls-proxy-secret @stored)))
        (is (true? (:enabled state)))
        (is (true? (:secret-configured state)))
        (is (not (contains? state :proxy-secret)) "The UI state never exposes the stored secret"))
      (auth/save-mtls-settings! {:trusted-cert-sha256 ""
                                 :proxy-secret ""
                                 :current-password "admin-password"})
      (is (= "" (:mtls-trusted-cert-sha256 @stored)) "An empty allowlist disables mTLS")
      (is (= secret (:mtls-proxy-secret @stored)) "A blank secret field keeps the stored secret")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"at least 32"
                            (auth/save-mtls-settings! {:trusted-cert-sha256 fingerprint
                                                       :proxy-secret "short"
                                                       :current-password "admin-password"})))
      (is (= secret (:mtls-proxy-secret @stored)) "Invalid input saves neither field")
      (auth/save-mtls-settings! {:trusted-cert-sha256 ""
                                 :proxy-secret ""
                                 :clear-proxy-secret "true"
                                 :current-password "admin-password"})
      (is (= "" (:mtls-proxy-secret @stored)) "The explicit delete control removes the secret"))
    (auth/initialize-mtls!))
  "The UI applies the same fail-closed validation as environment configuration")

(deftest environment-managed-mtls-cannot-be-overwritten-in-the-ui
  (with-redefs [auth/system-env #(get {"PLAUNA_MTLS_TRUSTED_CERT_SHA256" (apply str (repeat 32 "ab"))
                                      "PLAUNA_MTLS_PROXY_SECRET" (apply str (repeat 32 "s"))} %)
                auth/verify-web-password? (constantly true)
                settings/update-settings! (fn [_] (throw (ex-info "must not save" {})))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"managed by environment variables"
                          (auth/save-mtls-settings! {:trusted-cert-sha256 ""
                                                     :current-password "admin-password"}))))
  "Environment variables remain authoritative and make the UI read-only")

(deftest ui-mtls-changes-require-the-current-admin-password
  (let [saved? (atom false)]
    (with-redefs [auth/system-env (constantly nil)
                  auth/verify-web-password? (constantly false)
                  settings/update-settings! (fn [_] (reset! saved? true))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Current admin password is incorrect"
                            (auth/save-mtls-settings! {:trusted-cert-sha256 ""
                                                       :current-password "wrong"})))
      (is (false? @saved?))))
  "An authenticated session alone cannot modify mTLS access")

(deftest verified-settings-certificate-can-be-added-without-a-browser-supplied-fingerprint
  (let [existing-fingerprint (apply str (repeat 32 "ab"))
        request-fingerprint  (apply str (repeat 32 "cd"))
        secret               (apply str (repeat 32 "s"))
        stored               (atom {:mtls-trusted-cert-sha256 existing-fingerprint
                                    :mtls-proxy-secret secret})]
    (try
      (with-redefs [auth/system-env (constantly nil)
                    auth/verify-web-password? #(= "admin-password" %)
                    auth/verified-mtls-client-fingerprint (constantly request-fingerprint)
                    settings/fetch-setting #(get @stored %)
                    settings/update-settings! #(swap! stored merge %)]
        (auth/save-mtls-settings-from-request!
         {:params {:trusted-cert-sha256 existing-fingerprint
                   :proxy-secret ""
                   :current-password "admin-password"
                   :add-current-certificate "true"
                   :fingerprint "attacker-controlled"}})
        (is (= #{existing-fingerprint request-fingerprint}
               (set (str/split-lines (:mtls-trusted-cert-sha256 @stored)))))
        (is (not (str/includes? (:mtls-trusted-cert-sha256 @stored) "attacker-controlled"))))
      (finally
        (auth/initialize-mtls!))))
  "Certificate enrollment in settings derives identity solely from the verified request")

(deftest stored-ui-mtls-configuration-is-restored-at-startup
  (let [fingerprint (apply str (repeat 32 "ab"))
        secret      (apply str (repeat 32 "s"))]
    (try
      (with-redefs [auth/system-env (constantly nil)
                    settings/fetch-setting #(get {:mtls-trusted-cert-sha256 fingerprint
                                                  :mtls-proxy-secret secret} %)]
        (auth/initialize-mtls!)
        (is (true? (:enabled (auth/mtls-admin-state)))))
      (finally
        (auth/initialize-mtls!))))
  "UI-managed mTLS settings survive a normal Plauna restart")

(deftest login-name-mismatch-still-costs-a-password-check
  (let [checked (atom 0)]
    (with-redefs [settings/fetch-setting (fn [key] (when (= key :web-login-name) "alice"))
                  auth/verify-web-password? (fn [_] (swap! checked inc) true)]
      (is (false? (auth/verify-web-credentials? "root" "whatever")))
      (is (= 1 @checked) "A wrong login name does not short-circuit before the (slow) password check")))
  "The response time does not reveal whether the login name was right")

(deftest login-attempts-slow-down-after-failures
  (auth/record-login-success!)
  (is (zero? (auth/login-delay-millis)))
  (auth/record-login-failure!)
  (is (= 250 (auth/login-delay-millis)))
  (dotimes [_ 3] (auth/record-login-failure!))
  (is (= 4000 (auth/login-delay-millis)))
  (dotimes [_ 5] (auth/record-login-failure!))
  (is (= auth/max-login-delay-millis (auth/login-delay-millis)) "The brake is capped so the administrator is never locked out")
  (auth/record-login-success!)
  (is (zero? (auth/login-delay-millis)) "A successful login clears the brake"))

(deftest attempts-during-the-pause-are-refused-without-a-password-check
  (auth/record-login-success!)
  (is (zero? (auth/login-wait-remaining-millis)))
  (dotimes [_ 4] (auth/record-login-failure!))
  (let [remaining (auth/login-wait-remaining-millis)]
    (is (< 3500 remaining 4001) "Right after four failures the whole 4 s pause is still ahead"))
  (auth/record-login-success!)
  (is (zero? (auth/login-wait-remaining-millis))))

(deftest parallel-login-attempts-cannot-all-slip-through
  (auth/record-login-success!)
  (is (zero? (auth/claim-login-attempt!)) "The first attempt is checked")
  (is (pos? (auth/claim-login-attempt!)) "One arriving at the same time waits for the first one's pause")
  (auth/record-login-success!)
  (is (zero? (auth/claim-login-attempt!)) "A successful login clears the claim")
  (auth/record-login-success!))
