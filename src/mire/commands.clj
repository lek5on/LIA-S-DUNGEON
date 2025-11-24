(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]
            [mire.mobs :as mobs]
            [mire.items :as items]
            [mire.puzzles :as puzzles]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(def direction-labels
  {:north "север"
   :south "юг"
   :east "восток"
   :west "запад"})

(def direction-aliases
  {"north" :north "n" :north "север" :north "с" :north
   "south" :south "s" :south "юг" :south "ю" :south
   "east" :east "e" :east "восток" :east "в" :east
   "west" :west "w" :west "запад" :west "з" :west})

(defn- normalize-direction [direction]
  (let [dir (-> direction str str/lower-case)]
    (or (direction-aliases dir) (keyword dir))))

(def item-translations
  {:keys "ключи"
   :detector "детектор"
   :bunny "кролик"
   :turtle "черепаха"
   :puzzle "головоломка"
   :trader "торговец"})

(defn- item-name [item-kw]
  (let [ik (keyword item-kw)]
    (or (:name (items/get-weapon ik))
        (:name (items/get-armor ik))
        (:name (items/get-potion ik))
        (item-translations ik)
        (name ik))))

;; Command functions

(defn look
  "Получить описание комнаты и всего, что в ней есть."
  []
  (let [room @player/*current-room*
        exits (->> @(:exits room) keys (map #(get direction-labels % (name %))) (str/join ", "))
        items (seq @(:items room))
        mobs (seq @(:mobs room))
        others (disj @(:inhabitants room) player/*name*)]
    (str (:desc room)
         "\nВыходы: " exits "\n"
         (when items (str "Вы видите: " (str/join ", " (map item-name items)) "\n"))
         (when mobs (str "Противники: " (str/join ", " (map #(-> % deref :name) mobs)) "\n"))
         (when (seq others) (str "Здесь также: " (str/join ", " (map name others)) "\n")))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Куда идём?"
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (normalize-direction direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (look))
       "Туда не пройти."))))

(defn grab
  "Поднять предмет."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "Вы подобрали " (item-name thing) "."))
     (str "Здесь нет " (item-name thing) "."))))

(defn discard
  "Бросить предмет, который вы несёте."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "Вы бросили " (item-name thing) "."))
     (str "У вас нет " (item-name thing) "."))))

(defn inventory
  "Посмотреть свой инвентарь."
  []
  (str "У вас с собой:\n"
       (str/join "\n" (map item-name (seq @player/*inventory*)))))

(defn detect
  "Если у вас есть детектор, можно узнать, в какой комнате лежит предмет."
  [item]
  (let [item-kw (keyword item)]
    (if (@player/*inventory* :detector)
      (if-let [room (first (filter #((:items %) item-kw)
                                   (vals @rooms/rooms)))]
        (str (item-name item-kw) " находится в комнате \"" (rooms/room-title room) "\".")
        (str (item-name item-kw) " нет ни в одной комнате."))
      "Нужно нести детектор, чтобы это сделать.")))

(defn say
  "Сказать что-то вслух, чтобы услышали все в комнате."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println message)
        (println player/prompt)))
    (str "Вы сказали: " message)))

(defn help
  "Показать доступные команды и их описание."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

;; Command data

;; Game actions: combat, items, puzzles

(defn attack-mob
  "Атаковать моба. Команда: attack/атаковать [имя-моба]." [& [mob-name]]
  (let [pstats (or player/*stats* (player/init-stats 50 2))
        room @player/*current-room*
        target (or (mobs/find-mob-in-room room (when mob-name (keyword mob-name)))
                   (mobs/add-mob-to-room :zombie room))
        pmsg (mobs/player-attack-mob! pstats target)]
    (if (<= (:hp @target) 0)
      (do
        (player/add-xp! pstats (:xp @target))
        (player/heal! pstats (int (* 0.75 (:max-hp @pstats))))
        (mobs/remove-mob-from-room! target room)
        (str pmsg "\nВы убили " (:name @target) ". Вы получаете " (:xp @target) " опыта."))
      (str pmsg "\n" (mobs/mob-attack! target pstats)))))

(defn use-item
  "Использовать предмет из инвентаря. Команда: use/использовать <item>." [item]
  (let [pstats (or player/*stats* (player/init-stats 50 2))]
    (if (player/carrying? item)
      (do (dosync (alter player/*inventory* disj (keyword item)))
          (or (items/use-potion! pstats (keyword item))
              (str "Вы использовали " (item-name item) ", но ничего не произошло.")))
      (str "У вас нет " (item-name item) "."))))

(defn equip-item
  "Экипировать оружие. Команда: equip/экипировать <weapon>." [weapon]
  (let [pstats (or player/*stats* (player/init-stats 50 2))
        w (items/get-weapon (keyword weapon))]
    (if w
      (do (player/equip-weapon! pstats w)
          (str "Вы экипировали: " (:name w) "."))
      (str "Оружие не найдено: " weapon))))

(defn show-stats
  "Показать характеристики персонажа." []
  (str "Характеристики: " (pr-str @(or player/*stats* (player/init-stats 50 2)))))

(defn solve-puzzle
  "Посмотреть или решить загадку в текущей комнате. Команда: solve/решить <номер>."
  [choice]
  (let [room @player/*current-room*]
    (if-not (@(:items room) :puzzle)
      "Здесь нет загадки."
      (if (nil? choice)
        (let [p (puzzles/random-puzzle)]
          (str "Загадка: " (:q p) "\nВарианты: " (pr-str (:choices p)) "\nКоманда: solve/решить <номер-выбора>"))
        (let [idx (try (Integer/parseInt choice) (catch Exception _ nil))
              p (puzzles/random-puzzle)]
          (if (nil? idx)
            "Неверный выбор."
            (if (= idx (:answer p))
              (do (player/add-xp! (or player/*stats* (player/init-stats 50 2)) (:xp p))
                  (dosync (alter (:items room) disj :puzzle))
                  (str "Верно! Вы получаете " (:xp p) " опыта."))
              (do (player/damage! (or player/*stats* (player/init-stats 50 2))
                      (int (* 0.2 (:max-hp (or player/*stats* (player/init-stats 50 2))))))
                  (dosync (alter (:items room) disj :puzzle))
                  "Неправильно! Вы теряете 20% здоровья, и загадка исчезает."))))))))

(def commands {"move" move
               "идти" move
               "north" (fn [] (move :north))
               "south" (fn [] (move :south))
               "east" (fn [] (move :east))
               "west" (fn [] (move :west))
               "север" (fn [] (move :north))
               "юг" (fn [] (move :south))
               "восток" (fn [] (move :east))
               "запад" (fn [] (move :west))
               "grab" grab
               "взять" grab
               "поднять" grab
               "discard" discard
               "выбросить" discard
               "бросить" discard
               "inventory" inventory
               "инвентарь" inventory
               "attack" attack-mob
               "атаковать" attack-mob
               "атака" attack-mob
               "use" use-item
               "использовать" use-item
               "equip" equip-item
               "экипировать" equip-item
               "solve" solve-puzzle
               "загадка" solve-puzzle
               "решить" solve-puzzle
               "stats" show-stats
               "характеристики" show-stats
               "статы" show-stats
               "detect" detect
               "детектор" detect
               "look" look
               "осмотреться" look
               "say" say
               "сказать" say
               "help" help
               "помощь" help})

;; Command handling

(defn execute
  "Выполнить переданную команду."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "Так нельзя сделать!")))
