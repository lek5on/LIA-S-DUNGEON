(ns mire.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]
            [mire.items :as items]
            [mire.lobby :as lobby]))

(defn- cleanup []
  (lobby/leave! player/*name*)
  (lobby/unregister-player-stats! player/*name*)
  (when-let [room @player/*current-room*]
    (doseq [item @player/*inventory*]
      (commands/discard item))
    (dosync
     (commute (:inhabitants room) disj player/*name*)))
  (dosync
   (commute player/streams dissoc player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "Это имя занято, попробуйте другое: ")
        (flush)
        (recur (read-line)))
    name))

(defn- check-game-time []
  (when-let [end-time @lobby/game-end-time]
    (<= end-time (System/currentTimeMillis))))

(defn- show-game-over []
  ;; Wait a moment to let all players sync
  (Thread/sleep 500)
  (locking lobby/game-end-time
    (when @lobby/game-end-time
      (lobby/broadcast-game-over!)
      (reset! lobby/game-end-time nil)))
  (println "\n========================================")
  (println "         ВАШ ЛИЧНЫЙ РЕЗУЛЬТАТ")
  (println "========================================")
  (let [s @player/*stats*]
    (println (str "  Уровень: " (:level s)))
    (println (str "  XP: " (:xp s)))
    (println (str "  Золото: " (:gold s)))
    (println (str "  HP: " (:hp s) "/" (:max-hp s))))
  (println "========================================")
  (println "\nИгра окончена. Введите 'quit' для выхода.")
  (flush)
  (loop [line (read-line)]
    (when line
      (if (= (str/lower-case (str/trim line)) "quit")
        (println "До свидания!")
        (do
          (println "Введите 'quit' для выхода.")
          (flush)
          (recur (read-line)))))))

(defn- process-command [cmd menu]
  (cond
    (= cmd "quit")
    (do (println "До свидания!") (flush) :quit)

    (= cmd "look")
    (println (commands/execute "look"))

    (= cmd "inventory")
    (println (commands/execute "inventory"))

    (= cmd "stats")
    (println (commands/execute "stats"))

    (= cmd "timer")
    (println (commands/show-timer))

    (= cmd "levelup")
    (let [s @player/*stats*]
      (if-not (:pending-levelup s)
        (println "У вас нет доступных повышений уровня.")
        (do
          (println "=== ПОВЫШЕНИЕ УРОВНЯ! ===")
          (println "Выберите улучшение:")
          (println "1) +2 к урону")
          (println "2) +15 к максимальному HP")
          (print "Ваш выбор: ") (flush)
          (let [choice (str/trim (read-line))]
            (println (commands/execute (str "levelup " choice)))))))

    (= cmd "help")
    (println (commands/execute "help"))

    (= cmd "move")
    (let [exits @(:exits @player/*current-room*)]
      (if (empty? exits)
        (println "Отсюда нет выходов!")
        (do
          (println "Доступные направления:")
          (doseq [[idx [dir _]] (map-indexed vector (sort exits))]
            (println (str (inc idx) ") " (name dir))))
          (print "Выберите направление: ") (flush)
          (let [sel-str (str/trim (read-line))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                dir-choices (vec (map first (sort exits)))]
            (if (and sel (<= 1 sel) (<= sel (count dir-choices)))
              (println (commands/execute (str "move " (name (dir-choices (dec sel))))))
              (println "Неверный выбор."))))))

    (= cmd "attack")
    (let [mobs-here (seq @(:mobs @player/*current-room*))]
      (if (empty? mobs-here)
        (println "Здесь нет врагов.")
        (do
          (println "Враги в комнате:")
          (doseq [[idx mob-atom] (map-indexed vector mobs-here)]
            (let [m @mob-atom]
              (println (str (inc idx) ") " (:name m) " [HP: " (:hp m) "/" (:max-hp m) "]"))))
          (print "Выберите цель (или Enter для первого): ") (flush)
          (let [sel-str (str/trim (read-line))
                result (commands/execute "attack")]
            (println result)
            (when-not (player/alive? player/*stats*)
              (println "\n*** ИГРА ОКОНЧЕНА ***"))))))

    (= cmd "use")
    (let [inv (seq @player/*inventory*)
          usable (filter #(or (items/get-potion %) (= % :weapon-upgrade)) inv)]
      (if (empty? usable)
        (println "У вас нет предметов для использования.")
        (do
          (println "Доступные предметы:")
          (doseq [[idx item] (map-indexed vector usable)]
            (let [p (items/get-potion item)
                  item-name (cond
                              p (:name p)
                              (= item :weapon-upgrade) "Улучшение оружия (+2 урон)"
                              :else (name item))]
              (println (str (inc idx) ") " (name item) " - " item-name))))
          (print "Выберите предмет: ") (flush)
          (let [sel-str (str/trim (read-line))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                item-choices (vec usable)]
            (if (and sel (<= 1 sel) (<= sel (count item-choices)))
              (println (commands/execute (str "use " (name (item-choices (dec sel))))))
              (println "Неверный выбор."))))))

    (= cmd "equip")
    (let [inv (seq @player/*inventory*)
          weapons (filter #(items/get-weapon %) inv)
          armors (filter #(items/get-armor %) inv)
          equippable (concat weapons armors)]
      (if (empty? equippable)
        (println "У вас нет снаряжения в инвентаре.")
        (do
          (println "Снаряжение в инвентаре:")
          (doseq [[idx item] (map-indexed vector equippable)]
            (let [weapon (items/get-weapon item)
                  armor (items/get-armor item)]
              (cond
                weapon (println (str (inc idx) ") " (:name weapon) " [оружие, урон: " (:damage weapon) "]"))
                armor (println (str (inc idx) ") " (:name armor) " [броня, защита: " (:resist armor) "%]")))))
          (print "Выберите снаряжение: ") (flush)
          (let [sel-str (str/trim (read-line))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                equip-choices (vec equippable)]
            (if (and sel (<= 1 sel) (<= sel (count equip-choices)))
              (println (commands/execute (str "equip " (name (equip-choices (dec sel))))))
              (println "Неверный выбор."))))))

    (= cmd "grab")
    (let [items @(:items @player/*current-room*)]
      (if (empty? items)
        (println "Здесь нечего взять.")
        (do
          (println "Предметы в комнате:")
          (doseq [[idx item] (map-indexed vector (sort items))]
            (println (str (inc idx) ") " (name item))))
          (print "Выберите предмет: ") (flush)
          (let [sel-str (str/trim (read-line))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                item-choices (vec (sort items))]
            (if (and sel (<= 1 sel) (<= sel (count item-choices)))
              (println (commands/execute (str "grab " (name (item-choices (dec sel))))))
              (println "Неверный выбор."))))))

    (= cmd "trade")
    (let [traders (when-let [t (:traders @player/*current-room*)] @t)]
      (if (or (nil? traders) (empty? traders))
        (println "Здесь нет торговца.")
        (do
          (println (commands/execute "trade"))
          (print "Введите номер товара (или Enter для отмены): ") (flush)
          (let [sel-str (str/trim (read-line))]
            (when-not (empty? sel-str)
              (println (commands/execute (str "trade " sel-str))))))))

    (= cmd "puzzle")
    (if (some? (:puzzle @player/*current-room*))
      (do
        (println (commands/execute "solve"))
        (print "Ваш ответ (0, 1 или 2): ") (flush)
        (let [ans (str/trim (read-line))]
          (println (commands/execute (str "solve " ans)))))
      (println "Здесь нет загадки."))

    :else
    (println (commands/execute cmd)))
  nil)

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    (print "\nКак вас зовут? ") (flush)
    (binding [player/*name* (get-unique-player-name (read-line))
              player/*current-room* (ref nil)
              player/*inventory* (ref #{})
              player/*stats* (player/init-stats 100 4)]
      (try
        (dosync
         (commute player/streams assoc player/*name* *out*))

        (let [start-signal (lobby/register-player! player/*name*)
              poller (let [buf (StringBuilder.)]
                       (fn []
                         (when (.ready *in*)
                           (loop []
                             (let [ch (.read *in*)]
                               (cond
                                 (= ch -1) {:eof true}
                                 (or (= ch 10) (= ch 13))
                                 (let [s (.toString buf)]
                                   (.setLength buf 0)
                                   {:line s})
                                 :else (do (.append buf (char ch))
                                           (if (.ready *in*) (recur) nil))))))))]
          (println "\nДобро пожаловать в Mire," player/*name* "!")
          (println "Вы в лобби. Команды: ready, unready, status, quit.")
          (println (lobby/status))
          (print lobby/prompt) (flush)
          (loop []
            (cond
              (realized? start-signal)
              (println "\nВсе игроки готовы! Запуск подземелья...")

              :else
              (let [res (poller)]
                (cond
                  (nil? res)
                  (do (Thread/sleep 100) (recur))

                  (:eof res)
                  (do (println "До свидания!") (flush))

                  :else
                  (let [line (str/trim (:line res))
                        cmd (str/lower-case line)]
                    (case cmd
                      "ready" (println (lobby/mark-ready! player/*name* true))
                      "unready" (println (lobby/mark-ready! player/*name* false))
                      "status" (println (lobby/status))
                      "help" (println "Команды лобби: ready, unready, status, quit.")
                      "quit" (do (println "До свидания!") (flush))
                      (println "Неизвестная команда. Попробуйте: ready, unready, status, quit."))
                    (when-not (#{"quit"} cmd)
                      (print lobby/prompt) (flush)
                      (recur)))))))

          (when (realized? start-signal)
            (lobby/start-game-timer!)
            
            ;; Register player stats for live tracking
            (lobby/register-player-stats! player/*name* player/*stats*)
            
            (let [start-room (@rooms/rooms :start)]
              (dosync
               (ref-set player/*current-room* start-room)
               (commute (:inhabitants @player/*current-room*) conj player/*name*))
              (println (commands/look)))
            
            (println "\nВы начинаете игру с голыми кулаками (урон: 4).")
            (println "Найдите оружие и броню в подземелье или купите у торговца!")

            (let [menu {"1" "look"
                        "2" "move"
                        "3" "grab"
                        "4" "inventory"
                        "5" "attack"
                        "6" "use"
                        "7" "equip"
                        "8" "trade"
                        "9" "stats"
                        "*" "puzzle"
                        "#" "timer"
                        "+" "levelup"
                        "0" "quit"}]
              (println "\nКоманды: 1)Осмотр 2)Идти 3)Взять 4)Инвентарь 5)Атака 6)Исп. 7)Экип. 8)Торговля 9)Статы *)Загадка #)Таймер +)Уровень 0)Выход")
              (println (commands/show-timer))
              (print player/prompt) (flush)

              (loop [input (read-line)]
                (cond
                  (nil? input)
                  nil
                  
                  (not (player/alive? player/*stats*))
                  (do
                    (println "\n*** ВЫ ПОГИБЛИ ***")
                    (show-game-over))
                  
                  (check-game-time)
                  (show-game-over)
                  
                  :else
                  (let [trim (str/trim input)
                        cmd (get menu trim trim)
                        result (process-command cmd menu)]
                    (when (not= result :quit)
                      (when (:pending-levelup @player/*stats*)
                        (println "\n*** НОВЫЙ УРОВЕНЬ! Нажмите '+' чтобы выбрать улучшение ***"))
                      (print player/prompt) (flush)
                      (recur (read-line)))))))))
        (finally (cleanup))))))

(defn -main
  ([port dir use-procedural?]
   (rooms/add-rooms dir use-procedural?)
   (defonce server (socket/create-server (Integer. port) mire-handle-client))
   (println "Launching Mire server on port" port)
   (when use-procedural?
     (println "Procedural dungeon generation enabled")))
  ([port dir] (-main port dir true))
  ([port] (-main port "resources/rooms" true))
  ([] (-main 3333)))
