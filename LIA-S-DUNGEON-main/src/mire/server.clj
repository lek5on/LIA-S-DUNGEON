(ns mire.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]))

(def game-started? (atom false))
(def game-start-timer? (atom false))
(def match-end-time (atom nil))
(def match-announcer (atom nil))
(def base-hp 100)
(def base-damage 2)
(def match-duration-ms (* 5 60 1000))
(def announce-interval-ms (* 2 60 1000))

(def arrow->dir
  {\A "north"  ; up
   \B "south"  ; down
   \C "east"   ; right
   \D "west"}) ; left

(defn read-command!
  "Read next command from reader.
   - Enter-delimited text commands (trimmed). Single-char commands are lowercased.
   - Arrow keys (ESC [ A/B/C/D) are mapped to direction commands immediately.
   Returns nil on EOF."
  [^java.io.Reader rdr]
  (let [sb (StringBuilder.)]
    (loop [state nil]
      (let [ch (.read rdr)]
        (cond
          (= ch -1) nil

          (= state :esc)
          (case ch
            91 (recur :esc-bracket)          ; '['
            (do (.append sb (char 27))
                (.append sb (char ch))
                (recur nil)))

          (= state :esc-bracket)
          (if-let [cmd (arrow->dir (char ch))]
            cmd
            (recur nil))

          (= ch 27) (recur :esc)              ; ESC

          (or (= ch 10) (= ch 13))            ; newline / carriage return
          (let [s (str/trim (.toString sb))]
            (if (seq s)
              (if (= 1 (count s)) (str/lower-case s) s)
              (recur nil)))

          :else
          (do (.append sb (char ch))
              (recur nil)))))))
(def base-hp 100)
(def base-damage 2)

(defn all-ready? []
  (let [sessions @player/sessions]
    (and (seq sessions)
         (every? (fn [[_ {:keys [stats]}]]
                   (and stats (true? (:ready @stats))))
                 sessions))))

(defn broadcast [msg]
  (doseq [[_ stream] @player/streams]
    (binding [*out* stream]
      (println msg)
      (println player/prompt))))

(defn stop-match-announcer! []
  (when-let [f @match-announcer]
    (future-cancel f))
  (reset! match-announcer nil))

(defn start-match-announcer! [end-ts]
  (stop-match-announcer!)
  (reset! match-end-time end-ts)
  (reset! match-announcer
          (future
            (loop []
              (let [now (System/currentTimeMillis)
                    remain (- end-ts now)]
                (cond
                  (<= remain 0)
                  (do
                    (broadcast "Time is up! Match ended.")
                    (reset! game-started? false)
                    (reset! game-start-timer? false)
                    (reset! match-end-time nil)
                    (stop-match-announcer!))

                  :else
                  (do
                    (Thread/sleep (long (min announce-interval-ms remain)))
                    (let [min-left (Math/ceil (/ remain 60000.0))
                          sec-left (int (/ remain 1000))]
                      (broadcast (str "Time left: " min-left " min (" sec-left "s).")))
                    (recur))))))))

(defn- move-all-players! [target-room]
  (dosync
   (doseq [[p {:keys [room]}] @player/sessions]
     (when room (alter (:inhabitants @room) disj p)))
  (doseq [[p {:keys [room]}] @player/sessions]
    (when target-room
      (alter (:inhabitants target-room) conj p)
      (ref-set room target-room)))))

(defn- reset-player-for-start!
  "Return player to lobby with fresh stats/inventory for a fair race."
  [p {:keys [room stream]} lobby-room]
  (let [new-stats (player/init-stats base-hp base-damage)
        new-inventory (ref #{})]
    (dosync
     (alter (:inhabitants @room) disj p)
     (alter (:inhabitants lobby-room) conj p)
     (ref-set room lobby-room))
    (player/register-session! p stream room new-inventory new-stats)))

(defn start-game! []
  (when (and (not @game-started?) (seq @player/sessions))
    (reset! game-started? true)
    (reset! game-start-timer? false)
    (let [target-room (or (@rooms/rooms :start) (@rooms/rooms :lobby))
          lobby-room (@rooms/rooms :lobby)
          room-name (or (some-> target-room :name name) "the dungeon")
          end-ts (+ (System/currentTimeMillis) match-duration-ms)]
      (broadcast "Game starting now! All players reset.")
      (doseq [[p session] @player/sessions]
        (reset-player-for-start! p session lobby-room))
      (start-match-announcer! end-ts)
      (when target-room
        (move-all-players! target-room)
        (doseq [[p {:keys [inventory stats stream room]}] @player/sessions]
          (binding [player/*name* p
                    player/*inventory* inventory
                    player/*stats* stats
                    player/*current-room* room
                    *out* stream]
            (println (str "You have entered: " room-name))
            (println "Hint: move with arrows or w/a/s/d; use 'help' for commands.")
            (println (commands/look))
            (println player/prompt)))))))

(defn start-timer-if-needed []
  (when (and (not @game-started?) (not @game-start-timer?) (all-ready?))
    (reset! game-start-timer? true)
    (future
      (Thread/sleep 10000)
      (when (and (not @game-started?) (all-ready?))
        (start-game!))
      (reset! game-start-timer? false))))

(defn- cleanup []
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (alter (:inhabitants @player/*current-room*) disj player/*name*))
  (player/unregister-session! player/*name*))

(defn- acquire-player-name []
  (loop [candidate (str/trim (read-line))]
    (cond
      (str/blank? candidate)
      (do (print "Name cannot be blank; enter a name: ") (flush)
          (recur (str/trim (read-line))))

      (@player/streams candidate)
      (do (print "That name is already in use; try again: ") (flush)
          (recur (str/trim (read-line))))

      :else candidate)))

(defn- mire-handle-client [in out]
  (if @game-started?
    (binding [*out* (io/writer out)]
      (println "Game already started. New players cannot join.")
      (flush))
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    (print "\nWhat is your name? ") (flush)
      (binding [player/*name* (acquire-player-name)
                player/*current-room* (ref (@rooms/rooms :lobby))
                player/*inventory* (ref #{})
                player/*stats* (player/init-stats 100 2)]

        (dosync
         (commute (:inhabitants @player/*current-room*) conj player/*name*))
        (player/register-session! player/*name* *out* player/*current-room* player/*inventory* player/*stats*)

        (println "\nWelcome to Lobby," player/*name* "!")
        (println "Type 'ready' when you are ready to start.")
        (println "Move with arrows or w/a/s/d; other commands as text.")
        (println "Type 'help' to see commands. 'status' shows who's ready.")
        (println player/prompt)

        (try
          (loop [input (read-command! *in*)]
            (when input
              (let [cmd input]
                (cond
                  (= cmd "ready")
                  (do
                    (dosync (alter player/*stats* assoc :ready true))
                    (println "You are marked as ready.")
                    (broadcast (str player/*name* " is ready."))
                    (start-timer-if-needed))

                  :else
                  (println (commands/execute cmd))))

              (print player/prompt) (flush)
              (recur (read-command! *in*))))
          (finally (cleanup)))))))

(defn -main
  ([port dir]
   (rooms/add-rooms dir)
   (defonce server (socket/create-server (Integer. port) mire-handle-client))
   (println "Launching Mire server on port" port))

  ([port]
   (-main port "resources/rooms"))

  ([]
   (-main 3333)))
