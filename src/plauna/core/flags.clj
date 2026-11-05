(ns plauna.core.flags
  "Apple Mail flag colors. Apple Mail (macOS and iOS) stores a flag as the IMAP \\Flagged system flag
   and its color as a three-bit number in the keywords $MailFlagBit0..2. Red is the plain flag
   without any bit; other IMAP clients only see the flag itself.")

(def colors
  "The flag colors in Apple Mail's order: [{:id :label :bits :hex}], :hex roughly the color Apple Mail shows."
  [{:id "red"    :label "Red"    :bits #{} :hex "#ff3b30"}
   {:id "orange" :label "Orange" :bits #{0} :hex "#ff9500"}
   {:id "yellow" :label "Yellow" :bits #{1} :hex "#ffcc00"}
   {:id "green"  :label "Green"  :bits #{0 1} :hex "#34c759"}
   {:id "blue"   :label "Blue"   :bits #{2} :hex "#007aff"}
   {:id "purple" :label "Purple" :bits #{0 2} :hex "#af52de"}
   {:id "gray"   :label "Gray"   :bits #{1 2} :hex "#8e8e93"}])

(def color-keywords
  "The IMAP keywords that carry the color bits, by bit number."
  {0 "$MailFlagBit0" 1 "$MailFlagBit1" 2 "$MailFlagBit2"})

(defn valid-color
  "The flag color id when it is a known one, else nil (no flag)."
  [color]
  (some #(when (= color (:id %)) (:id %)) colors))

(defn keywords-for
  "[keywords-to-set keywords-to-clear] for a flag color id; an unknown id is the plain (red) flag."
  [color]
  (let [bits (or (:bits (first (filter #(= color (:id %)) colors))) #{})]
    [(set (map color-keywords bits))
     (set (map color-keywords (remove bits (keys color-keywords))))]))

(defn color-of
  "The flag color id that a set of IMAP keywords encodes for a flagged message."
  [keywords]
  (let [bits (set (keep (fn [[bit keyword]] (when (contains? keywords keyword) bit)) color-keywords))]
    (:id (first (filter #(= bits (:bits %)) colors)))))

(defn flag-change
  "What moving an e-mail from a category with flag color old-color to one with new-color (either nil
   for no flag) does to its flag: the new color, :clear when only the old category flagged its e-mails
   (Plauna's flag must not outlive the category it stood for), or nil to leave the flag alone - a flag
   the user set on an e-mail of an unflagged category is theirs."
  [old-color new-color]
  (cond (some? new-color) new-color
        (some? old-color) :clear
        :else nil))
