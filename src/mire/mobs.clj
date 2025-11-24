 (ns mire.mobs
  (:require [mire.player :as player]))

;; Definitions for mobs and simple combat helpers.

(def mobs
  {:zombie {:name "Zombie" :hp 30 :damage 5 :xp 20}
   :skeleton {:name "Skeleton" :hp 25 :damage 6 :xp 25}
   :witch {:name "Witch" :hp 40 :damage 8 :xp 40}
   :slenderman {:name "Slenderman" :hp 80 :damage 12 :xp 120}})

(defn spawn
  "Return an atom holding a mob instance cloned from the mob definition." [k]
  (when-let [m (get mobs (keyword k))]
    (atom (assoc m :id (keyword k) :hp (:hp m)))))

(defn add-mob-to-room
  "Create a mob instance for key `k` and add it to the room's :mobs ref.
   `room` should be a room map (not a ref). Returns the mob atom." [k room]
  (let [mob-atom (spawn k)]
    (dosync
      (alter (:mobs room) conj mob-atom))
    mob-atom))

(defn remove-mob-from-room!
  "Remove a mob-atom from a room's :mobs ref. Call inside a transaction or uses dosync.
   Returns true if removed." [mob-atom room]
  (dosync
   (alter (:mobs room) disj mob-atom)))

(defn find-mob-in-room
  "Find a mob atom in a room by keyword id or nil to return any mob." [room id]
  (let [all @(:mobs room)]
    (if id
      (first (filter #(= (:id @%) (keyword id)) all))
      (first all))))

(defn mob-attack!
  "Mob attacks a player stats ref. Uses player/damage! to apply damage."
  [mob-atom player-stats-ref]
  (let [dmg (:damage @mob-atom 0)]
    (player/damage! player-stats-ref dmg)
    (str (:name @mob-atom) " hits you for " dmg " damage.")))

(defn player-attack-mob!
  "Player (stats-ref) attacks mob-atom. Returns message and applies damage to mob." [player-stats mob-atom]
  (let [pdmg (:damage @player-stats)]
    (swap! mob-atom update :hp (fn [h] (max 0 (- h pdmg))))
    (str "You hit " (:name @mob-atom) " for " pdmg " damage.")))
