 (ns mire.items)

;; Basic items: weapons, armor, potions.

(def weapons
  {:fists {:name "Fists" :damage 1}
   :sword {:name "Sword" :damage 6}
   :club {:name "Club" :damage 4}
   :axe {:name "Axe" :damage 7}})

(def armors
  {:none {:name "Clothes" :resist 0}
   :leather {:name "Leather Armor" :resist 10}
   :chain {:name "Chainmail" :resist 20}})

(def potions
  {:hp-small {:name "Small HP Potion" :heal 20}
   :hp-medium {:name "Medium HP Potion" :heal 50}
   :resist {:name "Resistance Potion" :resist 25 :turns 3}})

(defn get-weapon [k]
  (when k (get weapons (keyword k))))

(defn get-armor [k]
  (when k (get armors (keyword k))))

(defn get-potion [k]
  (when k (get potions (keyword k))))

(defn weapon-damage [weapon]
  (or (:damage weapon) 0))

(defn armor-resist [armor]
  (or (:resist armor) 0))

(defn use-potion! [stats-ref potion]
  "Apply a potion effect to a player's stats ref. Currently only HP potions are supported."
  (when-let [p (get-potion potion)]
    (require 'mire.player)
    (if-let [heal (:heal p)]
      (do
        ((resolve 'mire.player/heal!) stats-ref heal)
        (str "You used " (:name p) " and recovered " heal " HP."))
      (if-let [res (:resist p)]
        (let [turns (:turns p 3)]
          ((resolve 'mire.player/apply-resist!) stats-ref res turns)
          (str "You used " (:name p) " and gained " res "% resistance for " turns " turns."))
        (str "You used " (:name p) ", but nothing happened.")))))
