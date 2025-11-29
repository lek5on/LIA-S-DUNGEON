(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]
            [mire.mobs :as mobs]
            [mire.items :as items]
            [mire.puzzles :as puzzles]
            [mire.lobby :as lobby]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (let [room @player/*current-room*
        exits (->> @(:exits room) keys (map name) (clojure.string/join ", "))
        items (seq @(:items room))
        mobs (seq @(:mobs room))
        puzzle (:puzzle room)
        traders (seq @(:traders room))
        others (disj @(:inhabitants room) player/*name*)]
    (str (:desc room)
         "\nВыходы: " exits "\n"
         (when items (str "Вы видите: " (clojure.string/join ", " (map name items)) "\n"))
         (when mobs (str "Враги здесь: " 
                         (clojure.string/join ", " 
                           (map #(let [m @%] (str (:name m) " [HP: " (:hp m) "/" (:max-hp m) "]")) mobs)) 
                         "\n"))
         (when puzzle (str "Вы замечаете загадочную надпись: \"" (:q puzzle) "\"\nИспользуйте: puzzle\n"))
         (when traders (str "Торговцы здесь: " (clojure.string/join ", " (map :name traders)) "\n"))
         (when (seq others) (str "Также здесь: " (clojure.string/join ", " (map name others)) "\n")))))

(defn move
  "Move in a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (look))
       "Вы не можете пойти туда."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "Вы подобрали " thing "."))
     (str "Здесь нет " thing "."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "Вы бросили " thing "."))
     (str "У вас нет " thing "."))))

(defn inventory
  "See what you've got."
  []
  (let [inv (seq @player/*inventory*)]
    (if inv
      (str "Вы несёте:\n" (str/join "\n" (map name inv)))
      "Ваш инвентарь пуст.")))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (player/carrying? "detector")
    (let [item-key (keyword item)]
      (if-let [room (first (filter #(rooms/room-contains? @(second %) item)
                                   @rooms/rooms))]
        (str item " находится в " (name (first room)))
        (str item " здесь нет")))
    "У вас нет детектора!"))

(defn use-item
  "Use an item in your inventory."
  [item]
  (if-not (player/carrying? item)
    (str "У вас нет " item ".")
    (let [item-key (keyword item)
          potion (items/get-potion item-key)]
      (cond
        ;; Weapon upgrade
        (= item-key :weapon-upgrade)
        (do
          (dosync (alter player/*inventory* disj item-key))
          (player/upgrade-weapon! player/*stats*)
          (str "Вы улучшили оружие! Урон увеличен на 2.\nВаш текущий урон: " (:damage @player/*stats*)))
        
        ;; Potion
        potion
        (do
          (dosync (alter player/*inventory* disj item-key))
          (if-let [heal (:heal potion)]
            (do
              (player/heal! player/*stats* heal)
              (str "Вы выпили " (:name potion) " и восстановили " heal " HP!\n"
                   "Ваше HP: " (:hp @player/*stats*) "/" (:max-hp @player/*stats*)))
            (if-let [res (:resist potion)]
              (let [turns (:turns potion 3)]
                (player/apply-resist! player/*stats* res turns)
                (str "Вы выпили " (:name potion) " и получили " res "% сопротивления на " turns " ходов!"))
              (str "Вы использовали " (:name potion) ", но ничего не произошло."))))
        
        :else
        (str "Вы не можете использовать " item ".")))))

(defn equip-item
  "Equip a weapon or armor from inventory."
  [item-name]
  (if-not (player/carrying? item-name)
    (str "У вас нет " item-name " в инвентаре.")
    (let [weapon (items/get-weapon item-name)
          armor (items/get-armor item-name)]
      (cond
        weapon
        (do
          (dosync (alter player/*inventory* disj (keyword item-name)))
          (player/equip-weapon! player/*stats* weapon)
          (str "Вы экипировали оружие: " (:name weapon) "."))
        
        armor
        (do
          (dosync (alter player/*inventory* disj (keyword item-name)))
          (player/equip-armor! player/*stats* armor)
          (str "Вы экипировали броню: " (:name armor) " (защита: " (:resist armor) "%)."))
        
        :else
        "Это нельзя экипировать."))))

(defn unequip
  "Remove your weapon."
  []
  (player/equip-weapon! player/*stats* nil)
  "Вы теперь без оружия.")

(defn display-stats
  "Show player stats."
  []
  (let [s @player/*stats*
        xp-needed (player/xp-for-level (:level s))
        weapon (get-in s [:slots :weapon])
        armor (get-in s [:slots :armor])]
    (str "=== Статистика ===\n"
         "HP: " (:hp s) "/" (:max-hp s) "\n"
         "Урон: " (:damage s) " (базовый: " (:base-damage s) ")\n"
         "Оружие: " (if weapon (:name weapon) "Кулаки") "\n"
         "Броня: " (if armor (str (:name armor) " (" (:resist armor) "%)") "Нет") "\n"
         "XP: " (:xp s) "/" xp-needed "\n"
         "Уровень: " (:level s) "\n"
         "Золото: " (:gold s)
         (when (:pending-levelup s)
           "\n\n*** НОВЫЙ УРОВЕНЬ! Используйте команду 'levelup' ***"))))

(defn levelup-choice
  "Apply level-up bonus based on choice."
  [choice]
  (let [s @player/*stats*]
    (if-not (:pending-levelup s)
      "У вас нет доступных повышений уровня."
      (case choice
        "1" (do (player/apply-level-up! player/*stats* :damage)
                (str "Вы выбрали +2 к урону!\nВаш новый базовый урон: " (:base-damage @player/*stats*)))
        "2" (do (player/apply-level-up! player/*stats* :hp)
                (str "Вы выбрали +15 к максимальному HP!\nВаше новое HP: " (:hp @player/*stats*) "/" (:max-hp @player/*stats*)))
        (str "Выберите улучшение:\n1) +2 к урону\n2) +15 к максимальному HP\nИспользуйте: levelup 1 или levelup 2")))))

(defn display-help
  "Get help."
  []
  "Исследуйте подземелье и получайте удовольствие!")

(defn say
  "Broadcast a message to the current room."
  [& words]
  (let [msg (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*) player/*name*)]
      (if-let [output (get @player/streams inhabitant)]
        (binding [*out* output]
          (println (str player/*name* " говорит: " msg))
          (flush))))
    (str "Вы говорите: " msg)))

(defn attack-mob
  "Attack an enemy in the current room. All mobs attack back."
  [& args]
  (let [room @player/*current-room*
        mob-seq (seq @(:mobs room))]
    (if-not (seq mob-seq)
      "Здесь нет врагов."
      (let [mob (if (empty? args)
                  (first mob-seq)
                  (mobs/find-mob-in-room room (first args)))]
        (if mob
          (do
            (mobs/player-attack-mob! player/*stats* mob)
            (let [m @mob]
              (if (<= (:hp m) 0)
                (let [gold-reward (or (:gold m) 10)
                      xp-reward (or (:xp m) 10)
                      ;; Drop potion: 65% hp-small, 35% hp-medium
                      potion-drop (if (< (rand) 0.65) :hp-small :hp-medium)
                      potion-name (if (= potion-drop :hp-small) "Малое зелье HP" "Среднее зелье HP")]
                  (player/add-gold! player/*stats* gold-reward)
                  (player/add-xp! player/*stats* xp-reward)
                  (dosync (alter player/*inventory* conj potion-drop))
                  (mobs/remove-mob-from-room! mob room)
                  ;; Remaining mobs still attack
                  (let [remaining-mobs (seq @(:mobs room))
                        counter-attacks (if remaining-mobs
                                          (str "\n" (mobs/all-mobs-attack! room player/*stats*))
                                          "")]
                    (if (player/alive? player/*stats*)
                      (str "Вы победили " (:name m) "! Получено " gold-reward " золота и " xp-reward " XP!\n"
                           "Выпало: " potion-name "!" counter-attacks
                           "\nВаше HP: " (:hp @player/*stats*) "/" (:max-hp @player/*stats*))
                      (str "Вы победили " (:name m) "!" counter-attacks "\n\n*** ВЫ ПОГИБЛИ ***"))))
                ;; All mobs attack back
                (let [attack-result (str "Вы ударили " (:name m) "! [HP: " (:hp m) "/" (:max-hp m) "]\n"
                                         "Все враги атакуют!\n" (mobs/all-mobs-attack! room player/*stats*))]
                  (if (player/alive? player/*stats*)
                    (str attack-result "\nВаше HP: " (:hp @player/*stats*) "/" (:max-hp @player/*stats*))
                    (str attack-result "\n\n*** ВЫ ПОГИБЛИ ***"))))))
          "Такого моба здесь нет!")))))

(defn solve-puzzle
  "Show or attempt to solve a puzzle in the current room."
  [& args]
  (let [room @player/*current-room*
        puzzle (:puzzle room)]
    (if-not puzzle
      "Здесь нет загадки."
      (if (empty? args)
        (str "Загадка: \"" (:q puzzle) "\"\n"
             "Варианты:\n"
             (str/join "\n" (map-indexed (fn [i c] (str i ") " c)) (:choices puzzle)))
             "\nИспользуйте: solve <номер>")
        (let [choice-idx (try (Integer/parseInt (first args)) (catch Exception _ nil))]
          (if (nil? choice-idx)
            "Введите номер (0, 1 или 2) для ответа."
            (if (<= choice-idx (dec (count (:choices puzzle))))
              (if (= choice-idx (:answer puzzle))
                (let [gold-reward (+ 20 (rand-int 31))
                      xp-reward (:xp puzzle)]
                  (player/add-gold! player/*stats* gold-reward)
                  (player/add-xp! player/*stats* xp-reward)
                  (str "Правильно! Вы решили загадку! Получено " xp-reward " XP и " gold-reward " золота!"))
                (str "Неправильно. Правильный ответ: " ((:choices puzzle) (:answer puzzle))))
              "Неверный номер варианта.")))))))

;; Trader shop items with prices
(def shop-items
  [{:id :hp-small :name "Малое зелье HP" :price 20}
   {:id :hp-medium :name "Среднее зелье HP" :price 50}
   {:id :resist :name "Зелье сопротивления" :price 40}
   {:id :sword :name "Меч" :price 80}
   {:id :axe :name "Топор" :price 100}
   {:id :leather :name "Кожаная броня" :price 60}
   {:id :chain :name "Кольчуга" :price 150}
   {:id :weapon-upgrade :name "Улучшение оружия (+2 урон)" :price 75}])

(defn trade
  "Trade with a merchant in the current room."
  [& args]
  (let [room @player/*current-room*
        traders (seq @(:traders room))]
    (if-not traders
      "Здесь нет торговца."
      (if (empty? args)
        (str "Добро пожаловать в магазин! Ваше золото: " (player/get-gold player/*stats*) "\n"
             "Товары:\n"
             (str/join "\n" (map-indexed (fn [i item] 
                                           (str i ") " (:name item) " - " (:price item) " золота")) 
                                         shop-items))
             "\nИспользуйте: trade <номер>")
        (let [choice-idx (try (Integer/parseInt (first args)) (catch Exception _ nil))]
          (if (nil? choice-idx)
            "Введите номер товара."
            (if (and (>= choice-idx 0) (< choice-idx (count shop-items)))
              (let [item (nth shop-items choice-idx)
                    price (:price item)]
                (if (player/spend-gold! player/*stats* price)
                  (do
                    (dosync (alter player/*inventory* conj (:id item)))
                    (str "Вы купили " (:name item) " за " price " золота!"))
                  (str "Недостаточно золота! Нужно " price ", у вас " (player/get-gold player/*stats*) ".")))
              "Неверный номер товара.")))))))

(defn show-timer
  "Show remaining game time."
  []
  (if-let [end-time @lobby/game-end-time]
    (let [remaining (- end-time (System/currentTimeMillis))
          minutes (quot remaining 60000)
          seconds (quot (mod remaining 60000) 1000)]
      (if (pos? remaining)
        (str "Осталось времени: " minutes ":" (format "%02d" seconds))
        "Время вышло!"))
    "Таймер не запущен."))

(def commands
  {"move" move
   "north" (fn [] (move "north"))
   "south" (fn [] (move "south"))
   "east" (fn [] (move "east"))
   "west" (fn [] (move "west"))
   "look" look
   "grab" grab
   "take" grab
   "drop" discard
   "discard" discard
   "inventory" inventory
   "detect" detect
   "use" use-item
   "equip" equip-item
   "unequip" unequip
   "stats" display-stats
   "help" display-help
   "say" say
   "attack" attack-mob
   "solve" solve-puzzle
   "trade" trade
   "timer" show-timer
   "levelup" levelup-choice})

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try
    (let [[command & args] (.split input " +")]
      (apply (commands command) args))
    (catch Exception e
      (.printStackTrace e (new java.io.PrintWriter *err*))
      "Вы не можете это сделать!")))
