(ns mire.items)

;; Базовые предметы: оружие, броня, зелья.

(def weapons
  {:fists {:name "Кулаки" :damage 1}
   :sword {:name "Меч" :damage 6}
   :club {:name "Дубинка" :damage 4}
   :axe {:name "Топор" :damage 7}})

(def armors
  {:none {:name "Одежда" :resist 0}
   :leather {:name "Кожаная броня" :resist 10}
   :chain {:name "Кольчуга" :resist 20}})

(def potions
  {:hp-small {:name "Малое зелье здоровья" :heal 20}
   :hp-medium {:name "Среднее зелье здоровья" :heal 50}
   :resist {:name "Зелье сопротивления" :resist 25 :turns 3}})

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
  "Применить эффект зелья к характеристикам игрока."
  (when-let [p (get-potion potion)]
    (require 'mire.player)
    (if-let [heal (:heal p)]
      (do
        ((resolve 'mire.player/heal!) stats-ref heal)
        (str "Вы использовали " (:name p) " и восстановили " heal " здоровья."))
      (if-let [res (:resist p)]
        (let [turns (:turns p 3)]
          ((resolve 'mire.player/apply-resist!) stats-ref res turns)
          (str "Вы использовали " (:name p) " и получили " res "% сопротивления на " turns " ходов."))
        (str "Вы использовали " (:name p) ", но ничего не произошло.")))))
