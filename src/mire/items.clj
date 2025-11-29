 (ns mire.items)

;; Basic items: weapons, armor, potions, upgrades.

(def weapons
  {:fists {:name "Кулаки" :damage 4 :min-damage 2 :max-damage 6}
   :sword {:name "Меч" :damage 6 :min-damage 4 :max-damage 8}
   :club {:name "Дубина" :damage 4 :min-damage 2 :max-damage 6}
   :axe {:name "Топор" :damage 7 :min-damage 5 :max-damage 9}})

(def armors
  {:none {:name "Одежда" :resist 0}
   :leather {:name "Кожаная броня" :resist 10}
   :chain {:name "Кольчуга" :resist 20}})

(def potions
  {:hp-small {:name "Малое зелье HP" :heal 20}
   :hp-medium {:name "Среднее зелье HP" :heal 50}
   :resist {:name "Зелье сопротивления" :resist 25 :turns 3}})

;; Weapon upgrade item
(def upgrade-kit
  {:name "Улучшение оружия" :damage-bonus 2})

(defn get-weapon [k]
  (when k (get weapons (keyword k))))

(defn get-armor [k]
  (when k (get armors (keyword k))))

(defn get-potion [k]
  (when k (get potions (keyword k))))

(defn is-upgrade? [k]
  (= (keyword k) :weapon-upgrade))

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
        (str "Вы выпили " (:name p) " и восстановили " heal " HP."))
      (if-let [res (:resist p)]
        (let [turns (:turns p 3)]
          ((resolve 'mire.player/apply-resist!) stats-ref res turns)
          (str "Вы выпили " (:name p) " и получили " res "% сопротивления на " turns " ходов."))
        (str "Вы использовали " (:name p) ", но ничего не произошло.")))))
