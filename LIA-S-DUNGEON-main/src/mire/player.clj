(ns mire.player)

;; Dynamic vars for each connected player session
(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *stats*)

;; Prompt symbol
(def prompt "> ")

;; Streams of players: {player-name -> output-stream}
(def streams (ref {}))
;; Sessions: {player-name {:room ref :inventory ref :stats ref :stream out-stream}}
(def sessions (ref {}))

;; PLAYER STATE HELPERS

(defn carrying? [thing]
  "Return true if the player has the item."
  (some #{(keyword thing)} @*inventory*))

(defn init-stats
  "Initialize stats for a player."
  [hp damage]
  (ref {:hp hp
        :max-hp hp
        :damage damage
        :base-damage damage
        :xp 0
        :level 1
        :slots {:weapon nil :armor nil :potions #{}}
        :resist_pct 0
        :resist_turns 0
        :ready false}))

(defn register-session!
  "Track a connected player's state so it can be accessed across threads."
  [name stream room inventory stats]
  (dosync
   (alter streams assoc name stream)
   (alter sessions assoc name {:room room
                               :inventory inventory
                               :stats stats
                               :stream stream})))

(defn unregister-session! [name]
  (dosync
   (alter sessions dissoc name)
   (alter streams dissoc name)))

(defn get-player-stats [pname]
  "Return stats ref of a player by name."
  (-> @sessions (get pname) :stats))

(defn session-by-name [pname]
  (@sessions pname))

(defn player-name-by-stream [stream]
  (first (for [[p s] @streams :when (= s stream)] p)))

(defn session-by-stream [stream]
  (when-let [pname (player-name-by-stream stream)]
    (session-by-name pname)))

;; HEAL / DAMAGE

(defn heal! [stats-ref amt]
  (dosync
   (alter stats-ref
          (fn [s]
            (assoc s :hp (min (:max-hp s)
                              (+ (:hp s) amt)))))))

(defn damage!
  "Deal damage after armor + resistance."
  [stats-ref amt]
  (dosync
   (alter stats-ref
          (fn [s]
            (let [armor (:armor (:slots s))
                  armor-resist (if armor (or (:resist armor) 0) 0)
                  resist-pct (or (:resist_pct s) 0)
                  total-resist (min 90 (+ armor-resist resist-pct))
                  effective-dmg (int (* amt (/ (- 100 total-resist) 100.0)))
                  new-hp (max 0 (- (:hp s) effective-dmg))
                  new-turns (if (pos? (:resist_turns s))
                              (dec (:resist_turns s))
                              0)
                  new-resist (if (pos? new-turns) resist-pct 0)]
              (-> s
                  (assoc :hp new-hp)
                  (assoc :resist_turns new-turns)
                  (assoc :resist_pct new-resist)))))) 

(defn alive? [stats-ref]
  (> (:hp @stats-ref) 0))

;; XP / LEVEL

(defn add-xp! [stats-ref amt]
  (dosync
   (alter stats-ref
          (fn [s]
            (let [new-xp (+ (:xp s) amt)
                  threshold (* 100 (:level s))]
              (if (>= new-xp threshold)
                (-> s
                    (assoc :xp (- new-xp threshold))
                    (update :level inc)
                    (update :max-hp + 10)
                    (update :hp + 10))
                (assoc s :xp new-xp))))))))

;; EQUIPMENT

(defn equip-weapon! [stats-ref weapon]
  (dosync
   (alter stats-ref
          (fn [s]
            (-> s
                (assoc-in [:slots :weapon] weapon)
                (assoc :damage (+ (:base-damage s)
                                  (or (:damage weapon) 0))))))))

(defn equip-armor! [stats-ref armor]
  (dosync
   (alter stats-ref assoc-in [:slots :armor] armor)))

;; RESIST

(defn apply-resist! [stats-ref pct turns]
  (dosync
   (alter stats-ref assoc :resist_pct pct)
   (alter stats-ref assoc :resist_turns turns)))

(defn stats-by-stream [stream]
  "Return the player's stats ref associated with output stream `stream`."
  (when-let [pname (player-name-by-stream stream)]
    (get-player-stats pname)))
