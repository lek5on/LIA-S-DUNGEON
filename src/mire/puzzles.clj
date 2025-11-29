(ns mire.puzzles)

;; Simple puzzle set. Each puzzle has :q question, :choices vector, :answer index, :xp reward

(def puzzles
  [;; Classic riddles
   {:id :p1 :q "Что ходит на четырёх ногах утром, на двух днём и на трёх вечером?"
    :choices ["Кошка" "Человек" "Стол"] :answer 1 :xp 30}
   {:id :p2 :q "Я говорю без рта и слышу без ушей. Что я?"
    :choices ["Эхо" "Ветер" "Дерево"] :answer 0 :xp 25}
   {:id :p3 :q "У чего есть клавиши, но нет замков?"
    :choices ["Пианино" "Карта" "Река"] :answer 0 :xp 20}
   {:id :p4 :q "Чем больше берёшь, тем больше становится. Что это?"
    :choices ["Деньги" "Яма" "Еда"] :answer 1 :xp 25}
   {:id :p5 :q "Что можно сломать, даже не прикасаясь?"
    :choices ["Стекло" "Обещание" "Лёд"] :answer 1 :xp 30}
   {:id :p6 :q "Что имеет голову и хвост, но не имеет тела?"
    :choices ["Змея" "Монета" "Комета"] :answer 1 :xp 20}
   {:id :p7 :q "Что становится мокрым, пока сушит?"
    :choices ["Солнце" "Полотенце" "Ветер"] :answer 1 :xp 25}
   {:id :p8 :q "Что принадлежит тебе, но другие используют чаще?"
    :choices ["Телефон" "Имя" "Деньги"] :answer 1 :xp 30}
   ;; Gaming references
   {:id :p9 :q "Сколько жизней у кошки?"
    :choices ["7" "9" "1"] :answer 1 :xp 15}
   {:id :p10 :q "Какой блок в Minecraft самый прочный?"
    :choices ["Алмазный" "Обсидиан" "Бедрок"] :answer 2 :xp 20}
   {:id :p11 :q "Кто главный злодей в серии игр Zelda?"
    :choices ["Боузер" "Ганондорф" "Сефирот"] :answer 1 :xp 25}
   {:id :p12 :q "Какая игра известна фразой 'The cake is a lie'?"
    :choices ["Half-Life" "Portal" "BioShock"] :answer 1 :xp 30}
   {:id :p13 :q "Сколько покемонов было в первом поколении?"
    :choices ["150" "151" "152"] :answer 1 :xp 20}
   ;; Math & Logic
   {:id :p14 :q "Если 2+2=4, то сколько будет 2×2?"
    :choices ["2" "4" "8"] :answer 1 :xp 15}
   {:id :p15 :q "Продолжите последовательность: 1, 1, 2, 3, 5, ?"
    :choices ["7" "8" "6"] :answer 1 :xp 25}
   {:id :p16 :q "Сколько сторон у додекаэдра?"
    :choices ["10" "12" "20"] :answer 1 :xp 30}
   ;; General knowledge
   {:id :p17 :q "Какая планета ближе всего к Солнцу?"
    :choices ["Венера" "Меркурий" "Марс"] :answer 1 :xp 20}
   {:id :p18 :q "Сколько цветов в радуге?"
    :choices ["6" "7" "8"] :answer 1 :xp 15}
   {:id :p19 :q "Какой химический элемент обозначается 'Au'?"
    :choices ["Серебро" "Золото" "Железо"] :answer 1 :xp 25}
   {:id :p20 :q "Кто написал 'Войну и мир'?"
    :choices ["Достоевский" "Толстой" "Чехов"] :answer 1 :xp 20}
   ;; Tricky riddles
   {:id :p21 :q "Что можно увидеть с закрытыми глазами?"
    :choices ["Ничего" "Сон" "Темноту"] :answer 1 :xp 30}
   {:id :p22 :q "У чего нет начала, конца и середины?"
    :choices ["Круг" "Линия" "Точка"] :answer 0 :xp 25}
   {:id :p23 :q "Что всегда идёт, но никуда не уходит?"
    :choices ["Река" "Время" "Дорога"] :answer 1 :xp 30}])

(defn random-puzzle []
  (let [p (rand-nth puzzles)]
    (dissoc p :id)))
