 (ns mire.puzzles)

;; Simple puzzle set. Each puzzle has :q question, :choices vector, :answer index, :xp reward

(def puzzles
  [{:id :p1 :q "What walks on four legs in the morning, two at noon, and three in the evening?"
    :choices ["Cat" "Human" "Table"] :answer 1 :xp 30}
   {:id :p2 :q "I speak without a mouth and hear without ears. What am I?"
    :choices ["Echo" "Wind" "Tree"] :answer 0 :xp 25}
   {:id :p3 :q "What has keys but can't open locks?"
    :choices ["Piano" "Map" "River"] :answer 0 :xp 20}])

(defn random-puzzle []
  (let [p (rand-nth puzzles)]
    (dissoc p :id)))
