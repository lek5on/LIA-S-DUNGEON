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
    (do (print "That name is in use; try again: ")
        (flush)
        (recur (read-line)))
    name))

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    (print "\nWhat is your name? ") (flush)
    (binding [player/*name* (get-unique-player-name (read-line))
              player/*current-room* (ref (@rooms/rooms :start))
              player/*inventory* (ref #{})
              player/*stats* (player/init-stats 100 2)]
      (dosync
       (commute (:inhabitants @player/*current-room*) conj player/*name*)
       (commute player/streams assoc player/*name* *out*))

      ;; Welcome, show room and allow choosing a starting weapon
      (println "\nWelcome to Mire," player/*name* "!")
      (println (commands/look))
      (println "Choose your starting weapon:")
      (doseq [[idx w] (map-indexed vector (keys items/weapons))]
        (println (str (inc idx) ") " (name w))))
      (print "Enter number (or blank to skip): ") (flush)
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
                  "0" "quit"}]
        (println "\nCommands: 1)Look 2)Move 3)Inventory 4)Attack 5)Use 6)Equip 7)Stats 8)Help 9)Say 0)Quit")
        (print player/prompt) (flush)

        (try
          (loop [input (read-line)]
            (when input
              (let [trim (clojure.string/trim input)
                    cmd (get menu trim trim)]
                (cond
                  (= cmd "quit")
                  (do (println "Goodbye!") (flush))

                  (= cmd "look")
                  (println (commands/execute "look"))

                  (= cmd "inventory")
                  (println (commands/execute "inventory"))

                  (= cmd "stats")
                  (println (commands/execute "stats"))

                  (= cmd "help")
                  (println (commands/execute "help"))

                  (= cmd "move")
                  (do (print "Direction (north/south/east/west): ") (flush)
                      (let [d (clojure.string/trim (read-line))]
                        (println (commands/execute (str "move " d)))))

                  (= cmd "attack")
                  (do (print "Mob name (optional): ") (flush)
                      (let [m (clojure.string/trim (read-line))]
                        (println (commands/execute (if (empty? m) "attack" (str "attack " m))))))

                  (= cmd "use")
                  (do (print "Item name: ") (flush)
                      (let [i (clojure.string/trim (read-line))]
                        (println (commands/execute (str "use " i)))))

                  (= cmd "equip")
                  (do (println "Choose weapon:")
                      (doseq [[idx w] (map-indexed vector (keys items/weapons))]
                        (println (str (inc idx) ") " (name w))))
                      (print "Enter number: ") (flush)
                      (let [sel (try (Integer/parseInt (read-line)) (catch Exception _ nil))
                            choices (vec (keys items/weapons))]
                        (when (and sel (<= 1 sel) (<= sel (count choices)))
                          (println (commands/execute (str "equip " (name (choices (dec sel)))))))))

                  (= cmd "say")
                  (do (print "Say: ") (flush)
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
     (println "Launching Mire server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))
