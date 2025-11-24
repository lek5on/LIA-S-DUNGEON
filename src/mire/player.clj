(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *stats*)

(def prompt "> ")
(def streams (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))

(defn init-stats
  "Create a new stats ref for a player with base hp and damage. Returns a ref." 
  [hp damage]
  (ref {:hp hp
        :max-hp hp
        :damage damage
        :xp 0
        :level 1
  :slots {:weapon nil :armor nil :potions #{}}
  :resist_pct 0
  :resist_turns 0}))

(defn heal! [stats-ref amt]
  (dosync
   (alter stats-ref (fn [s]
                      (let [new-hp (min (:max-hp s) (+ (:hp s) amt))]
                        (assoc s :hp new-hp))))))

(defn damage! [stats-ref amt]
  (dosync
   (alter stats-ref (fn [s]
                      (let [; base armor resist from equipped armor
                            armor (:armor (:slots s))
                            armor-resist (if armor (or (:resist armor) 0) 0)
                            resist-pct (or (:resist_pct s) 0)
                            total-resist (min 90 (+ armor-resist resist-pct))
                            effective-dmg (int (Math/round (* amt (/ (- 100 total-resist) 100.0))))
                            new-hp (max 0 (- (:hp s) effective-dmg))
                            ; decrement resist turns if present
                            new-resist-turns (if (and (pos? (:resist_turns s)) (pos? resist-pct))
                                               (dec (:resist_turns s))
                                               (:resist_turns s))
                            new-resist-pct (if (<= (or new-resist-turns 0) 0) 0 resist-pct)]
                        (-> s
                            (assoc :hp new-hp)
                            (assoc :resist_turns new-resist-turns)
                            (assoc :resist_pct new-resist-pct)))))))

(defn alive? [stats-ref]
  (> (:hp @stats-ref) 0))

(defn add-xp! [stats-ref amt]
  (dosync
   (alter stats-ref (fn [s]
                      (let [new-xp (+ (:xp s) amt)
                            threshold (* 100 (:level s))]
                        (if (>= new-xp threshold)
                          (-> s
                              (assoc :xp (- new-xp threshold))
                              (update :level inc)
                              (update :max-hp (fn [m] (+ m 10)))
                              (assoc :hp (+ (:hp s) 10)))
                          (assoc s :xp new-xp)))))))

(defn equip-weapon! [stats-ref weapon]
  (dosync
   (alter stats-ref assoc-in [:slots :weapon] weapon)
   (alter stats-ref (fn [s]
                      (assoc s :damage (+ (:damage s) (or (:damage weapon) 0)))))))

(defn equip-armor! [stats-ref armor]
  (dosync
   (alter stats-ref assoc-in [:slots :armor] armor)))

(defn apply-resist! [stats-ref pct turns]
  "Apply a percentage damage resistance for a number of turns to stats-ref."
  (dosync
   (alter stats-ref assoc :resist_pct pct)
   (alter stats-ref assoc :resist_turns turns)))
