 (ns mire.server
   (:require [clojure.java.io :as io]
             [server.socket :as socket]
             [mire.player :as player]
             [mire.commands :as commands]
             [mire.rooms :as rooms]
             [mire.items :as items]))

(defn- cleanup []
  "Drop all inventory and remove player from room and player list."
 (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "Это имя уже занято; попробуйте другое: ")
        (flush)
        (recur (read-line)))
    name))

(defn- weapon-title [w]
  (or (:name (items/get-weapon w)) (name w)))

(defn- print-weapon-options []
  (doseq [[idx w] (map-indexed vector (keys items/weapons))]
    (println (str (inc idx) ") " (weapon-title w) " (" (name w) ")"))))

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    (print "\nКак тебя зовут? ") (flush)
    (binding [player/*name* (get-unique-player-name (read-line))
              player/*current-room* (ref (@rooms/rooms :start))
              player/*inventory* (ref #{})
              player/*stats* (player/init-stats 100 2)]
      (dosync
       (commute (:inhabitants @player/*current-room*) conj player/*name*)
       (commute player/streams assoc player/*name* *out*))

      ;; Welcome, show room and allow choosing a starting weapon
      (println "\nДобро пожаловать в Mire," player/*name* "!")
      (println (commands/look))
      (println "Выберите стартовое оружие:")
      (print-weapon-options)
      (print "Введите номер (или оставьте пустым, чтобы пропустить): ") (flush)
      (let [sel (try (Integer/parseInt (read-line)) (catch Exception _ nil))
            choices (vec (keys items/weapons))]
        (when (and sel (<= 1 sel) (<= sel (count choices)))
          (let [weapon (name (choices (dec sel)))]
            (println (commands/execute (str "equip " weapon))))))

      ;; Minimal numeric menu mapping
      (let [menu {"1" "look"
                  "2" "move"
                  "3" "inventory"
                  "4" "attack"
                  "5" "use"
                  "6" "equip"
                  "7" "stats"
                  "8" "help"
                  "9" "say"
                  "0" "quit"
                  "выйти" "quit"
                  "выход" "quit"}]
        (println "\nКоманды: 1)Осмотреться 2)Идти 3)Инвентарь 4)Атаковать 5)Использовать 6)Экипировать 7)Статы 8)Помощь 9)Сказать 0)Выйти")
        (print player/prompt) (flush)

        (try
          (loop [input (read-line)]
            (when input
              (let [trim (clojure.string/trim input)
                    cmd (get menu trim trim)]
                (cond
                  (= cmd "quit")
                  (do (println "До встречи!") (flush))

                  (= cmd "look")
                  (println (commands/execute "look"))

                  (= cmd "inventory")
                  (println (commands/execute "inventory"))

                  (= cmd "stats")
                  (println (commands/execute "stats"))

                  (= cmd "help")
                  (println (commands/execute "help"))

                  (= cmd "move")
                  (do (print "Куда идти (север/юг/восток/запад): ") (flush)
                      (let [d (clojure.string/trim (read-line))]
                        (println (commands/execute (str "move " d)))))

                  (= cmd "attack")
                  (do (print "Имя врага (опционально): ") (flush)
                      (let [m (clojure.string/trim (read-line))]
                        (println (commands/execute (if (empty? m) "attack" (str "attack " m))))))

                  (= cmd "use")
                  (do (print "Название предмета: ") (flush)
                      (let [i (clojure.string/trim (read-line))]
                        (println (commands/execute (str "use " i)))))

                  (= cmd "equip")
                  (do (println "Выберите оружие:")
                      (print-weapon-options)
                      (print "Введите номер: ") (flush)
                      (let [sel (try (Integer/parseInt (read-line)) (catch Exception _ nil))
                            choices (vec (keys items/weapons))]
                        (when (and sel (<= 1 sel) (<= sel (count choices)))
                          (println (commands/execute (str "equip " (name (choices (dec sel)))))))))

                  (= cmd "say")
                  (do (print "Сказать: ") (flush)
                      (let [msg (read-line)] (println (commands/execute (str "say " msg)))))

                  :else
                  (println (commands/execute cmd))))

              (print player/prompt) (flush)
              (recur (read-line))))
          (finally (cleanup)))))))

(defn -main
  ([port dir]
     (rooms/add-rooms dir)
     (defonce server (socket/create-server (Integer. port) mire-handle-client))
     (println "Запускаю сервер Mire на порту" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))
