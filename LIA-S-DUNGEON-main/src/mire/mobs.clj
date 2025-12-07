(ns mire.mobs
  (:require [mire.player :as player]))

(def mobs
  {:zombie     {:name "Zombie"     :hp 30 :damage 5  :xp 20}
   :skeleton   {:name "Skeleton"   :hp 25 :damage 6  :xp 25}
   :witch      {:name "Witch"      :hp 40 :damage 8  :xp 40}
   :slenderman {:name "Slenderman" :hp 80 :damage 12 :xp 120}})

(defn spawn [k]
  (when-let [m (get mobs (keyword k))]
    (atom (assoc m :id (keyword k) :hp (:hp m)))))

(defn add-mob-to-room [k room]
  (let [mob-atom (spawn k)]
    (dosync (alter (:mobs room) conj mob-atom))
    mob-atom))

(defn remove-mob-from-room! [mob-atom room]
  (dosync (alter (:mobs room) disj mob-atom)))

(defn find-mob-in-room [room id]
  (let [all @(:mobs room)]
    (if id
      (first (filter #(= (:id @%) (keyword id)) all))
      (first all))))

(defn mob-attack! [mob-atom player-stats-ref]
  (let [dmg (:damage @mob-atom)]
    (player/damage! player-stats-ref dmg)
    (str (:name @mob-atom) " hits you for " dmg " damage.")))

(defn player-attack-mob! [player-stats mob-atom]
  (let [pdmg (:damage @player-stats)]
    (swap! mob-atom update :hp #(max 0 (- % pdmg)))
    (str "You hit " (:name @mob-atom) " for " pdmg " damage.")))
