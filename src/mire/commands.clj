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

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (let [room @player/*current-room*
        exits (->> @(:exits room) keys (map name) (clojure.string/join ", "))
        items (seq @(:items room))
        mobs (seq @(:mobs room))
        others (disj @(:inhabitants room) player/*name*)]
    (str (:desc room)
         "\nExits: " exits "\n"
         (when items (str "You see: " (clojure.string/join ", " (map name items)) "\n"))
         (when mobs (str "Enemies here: " (clojure.string/join ", " (map #(-> % deref :name) mobs)) "\n"))
         (when (seq others) (str "Also here: " (clojure.string/join ", " (map name others)) "\n")))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
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
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up the " thing "."))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println message)
        (println player/prompt)))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

(defn lobby-status
  "Show lobby readiness information."
  []
  (lobby/status))

;; Command data

;; Game actions: combat, items, puzzles

(defn attack-mob
  "Attack a mob. Usage: attack [mobname]." [& [mob-name]]
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
        (str pmsg "\nYou killed " (:name @target) ". You gain " (:xp @target) " XP."))
      (str pmsg "\n" (mobs/mob-attack! target pstats)))))

(defn use-item
  "Use an item from inventory. Usage: use <item>" [item]
  (let [pstats (or player/*stats* (player/init-stats 50 2))]
    (if (player/carrying? item)
      (do (dosync (alter player/*inventory* disj (keyword item)))
          (or (items/use-potion! pstats (keyword item))
              (str "You used " item ", but nothing happened.")))
      (str "You're not carrying " item))))

(defn equip-item
  "Equip a weapon. Usage: equip <weapon>" [weapon]
  (let [pstats (or player/*stats* (player/init-stats 50 2))
        w (items/get-weapon (keyword weapon))]
    (if w
      (do (player/equip-weapon! pstats w)
          (str "You equipped " (:name w) "."))
      (str "No such weapon: " weapon))))

(defn show-stats
  "Show player stats." []
  (str "Stats: " (pr-str @(or player/*stats* (player/init-stats 50 2)))))

(defn solve-puzzle
  "Show or attempt to solve a puzzle in the current room."
  [choice]
  (let [room @player/*current-room*]
    (if-not (@(:items room) :puzzle)
      "There is no puzzle here."
      (if (nil? choice)
        (let [p (puzzles/random-puzzle)]
          (str "Puzzle: " (:q p) "\nChoices: " (pr-str (:choices p)) "\nUse: solve <choice-index>"))
        (let [idx (try (Integer/parseInt choice) (catch Exception _ nil))
              p (puzzles/random-puzzle)]
          (if (nil? idx)
            "Invalid choice"
            (if (= idx (:answer p))
              (do (player/add-xp! (or player/*stats* (player/init-stats 50 2)) (:xp p))
                  (dosync (alter (:items room) disj :puzzle))
                  (str "Correct! You gain " (:xp p) " XP."))
              (do (player/damage! (or player/*stats* (player/init-stats 50 2))
                      (int (* 0.2 (:max-hp (or player/*stats* (player/init-stats 50 2))))))
                  (dosync (alter (:items room) disj :puzzle))
                  "Wrong! You take 20% damage and the puzzle disappears."))))))))

(def commands {"move" move
               "north" (fn [] (move :north))
               "south" (fn [] (move :south))
               "east" (fn [] (move :east))
               "west" (fn [] (move :west))
               "grab" grab
               "discard" discard
               "inventory" inventory
               "attack" attack-mob
               "use" use-item
               "equip" equip-item
               "solve" solve-puzzle
               "stats" show-stats
               "detect" detect
               "look" look
               "say" say
               "help" help
               "status" lobby-status})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))
