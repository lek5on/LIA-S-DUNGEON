(ns mire.puzzles)

(def puzzles
  [{:id :p1
    :type :riddle
    :q "Что ходит утром на четырёх ногах, днём на двух, вечером на трёх?"
    :choices ["Стол" "Человек" "Собака"]
    :answer 1
    :xp 30}

   {:id :p2
    :type :cipher
    :q "Шифр Цезаря сдвиг 1: \"Dppe\". Что это?"
    :choices ["Coffee" "Apple" "Bread"]
    :answer 0
    :xp 35}

   {:id :p3
    :type :analogy
    :q "Огонь : тепло :: лёд : ?"
    :choices ["Холод" "Вода" "Пар"]
    :answer 0
    :xp 25}

   {:id :p4
    :type :riddle
    :q "Без окон, без дверей — полна горница людей. Что это?"
    :choices ["Огурец" "Опера" "Банка"]
    :answer 0
    :xp 20}

   {:id :p5
    :type :cipher
    :q "Анаграмма: «МОРЯК» — это?"
    :choices ["Ромка" "Кором" "Мярок"]
    :answer 0
    :xp 30}])

(defn random-puzzle []
  (let [p (rand-nth puzzles)]
    p))
