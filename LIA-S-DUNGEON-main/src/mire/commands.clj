(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]
            [mire.mobs :as mobs]
            [mire.items :as items]
            [mire.puzzles :as puzzles]))

(def solve-time-limit-ms 30000)
(def solve-fast-bonus-ms 5000)
(def solve-medium-bonus-ms 15000)

(def command-help
  [["look" "Осмотреть комнату (подсказки по паззлам, выходам, мобам)."]
   ["move <dir>" "Пойти north/south/east/west (стрелки или w/a/s/d тоже работают)."]
   ["inventory" "Показать, что несёшь."]
   ["grab <item>" "Подобрать предмет в комнате."]
   ["discard <item>" "Выкинуть предмет из инвентаря."]
   ["attack [mob]" "Атаковать моба (или указанного по имени)."]
   ["use <potion>" "Выпить зелье (хил/резист)."]
   ["equip <weapon>" "Экипировать оружие."]
   ["stats" "Показать текущие статы."]
   ["detect <item>" "Найти комнату с предметом (нужен detector)."]
  ["solve [num]" "Решить паззл в комнате: сначала `solve`, потом `solve 2` и т.п."]
   ["say <text>" "Сказать всем в комнате."]
   ["status" "В лобби: кто в игре и кто готов."]
   ["ready" "В лобби: отметить готовность к старту."]
   ["help" "Список команд."]])

(defn- move-between-refs
  "Move one instance of obj between `from` and `to`. Must be called inside a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

;; LOOK

(defn look []
  "Look around the current room."
  (let [room @player/*current-room*
        exits (->> @(:exits room) keys (map name) (str/join ", "))
        items (seq @(:items room))
        mobs (seq @(:mobs room))
        others (disj @(:inhabitants room) player/*name*)
        puzzle? (rooms/puzzle-available-for? room player/*name*)]
    (str (:desc room) "\n"
         "Exits: " exits "\n"
         (when items
           (str "You see: " (str/join ", " (map name items)) "\n"))
         (when mobs
           (str "Enemies here: "
                (str/join ", " (map #(-> % deref :name) mobs)) "\n"))
         (when puzzle?
           "There is a puzzle here. Type 'solve' to attempt it.\n")
         (when (seq others)
           (str "Also here: " (str/join ", " (map name others)) "\n")))))

;; MOVE

(defn move [direction]
 "Move in a direction."
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do (move-between-refs player/*name*
                              (:inhabitants @player/*current-room*)
                              (:inhabitants target))
           (ref-set player/*current-room* target)
           (look))
       "You can't go that way."))))

;; INVENTORY

(defn grab [thing]
  "Pick up an item."
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up the " thing "."))
     (str "There is no " thing " here."))))

(defn discard [thing]
  "Drop an item."
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You are not carrying a " thing "."))))

(defn inventory []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))))

;; DETECTOR

(defn detect [item]
  "Locate an item in the world."
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room) ".")
      (str item " is not in any room."))
    "You need a detector to use this."))

;; SAY

(defn say [& words]
  "Say something to everyone in the room."
  (let [msg (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*) player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println msg)
        (println player/prompt)))
    (str "You said: " msg)))

;; COMBAT

(defn attack-mob [& [mob-name]]
  "Attack a mob."
  (let [pstats player/*stats*
        room @player/*current-room*
        target (or (mobs/find-mob-in-room room (when mob-name (keyword mob-name)))
                   (mobs/add-mob-to-room :zombie room))
        msg (mobs/player-attack-mob! pstats target)]
    (if (<= (:hp @target) 0)
      (do (player/add-xp! pstats (:xp @target))
          (player/heal! pstats (int (* 0.75 (:max-hp @pstats))))
          (mobs/remove-mob-from-room! target room)
          (str msg "\nYou killed " (:name @target)
               ". You gain " (:xp @target) " XP."))
      (str msg "\n" (mobs/mob-attack! target pstats)))))

(defn use-item [item]
  "Use an item."
  (let [pstats player/*stats*]
    (if (player/carrying? item)
      (do (dosync (alter player/*inventory* disj (keyword item)))
          (items/use-potion! pstats (keyword item)))
      (str "You are not carrying " item "."))))

(defn equip-item [weapon]
  "Equip a weapon."
  (let [pstats player/*stats*
        w (items/get-weapon (keyword weapon))]
    (if w
      (do (player/equip-weapon! pstats w)
          (str "You equipped " (:name w) "."))
      (str "No such weapon: " weapon))))

(defn show-stats []
  (str "Stats: " (pr-str @player/*stats*)))

(defn solve-puzzle
  "Attempt to solve a room puzzle once. Provide answer as `solve <number>`."
  [& [answer-str]]
  (let [room @player/*current-room*
        now (System/currentTimeMillis)]
    (cond
      (not (rooms/puzzle-present? room))
      "No puzzle here."

      (not (rooms/puzzle-available-for? room player/*name*))
      "You already attempted this puzzle."

      :else
      (let [{:keys [q choices answer xp type]} (rooms/get-puzzle room)
            {:keys [deadline started]} (rooms/puzzle-timer room player/*name*)
            started (or started now)
            deadline (or deadline (+ started solve-time-limit-ms))]
        (when (nil? deadline)
          (rooms/set-puzzle-timer! room player/*name* started (+ started solve-time-limit-ms)))
        (cond
          (> now deadline)
          (do (rooms/mark-puzzle-attempted! room player/*name*)
              "Time is up. Puzzle closed.")

          (nil? answer-str)
          (let [remaining (max 0 (quot (- deadline now) 1000))
                opts (str/join "\n" (for [[idx ch] (map-indexed vector choices)]
                                      (str (inc idx) ") " ch)))]
            (str "Puzzle (" (name (or type :riddle)) "): " q "\n"
                 opts "\n"
                 "Use `solve <number>` within " remaining "s. "
                 "Бонус за скорость: <5с = +50% XP, <15с = +20% XP." ))

          :else
          (let [sel (try (some-> answer-str str/trim Integer/parseInt dec)
                         (catch Exception _ nil))
                elapsed (- now started)
                mult (cond
                       (<= elapsed solve-fast-bonus-ms) 1.5
                       (<= elapsed solve-medium-bonus-ms) 1.2
                       :else 1.0)
                reward (int (Math/round (* xp mult)))]
            (rooms/mark-puzzle-attempted! room player/*name*)
            (if (and (number? sel) (= sel answer))
              (do (player/add-xp! player/*stats* reward)
                  (str "Correct! You gain " reward " XP."))
              "Wrong answer. You can't retry this puzzle.")))))))

(defn lobby-status []
  "Show who is connected and their readiness; lobby-only."
  (if (= :lobby (:name @player/*current-room*))
    (let [sessions @player/sessions
          total (count sessions)
          ready-count (count (filter (fn [[_ {:keys [stats]}]]
                                       (true? (:ready @stats)))
                                     sessions))
          entries (for [[p {:keys [stats]}] sessions]
                    (str p " - " (if (:ready @stats) "ready" "not ready")))]
      (if (seq sessions)
        (str "Players: " (str/join ", " entries)
             "\nReady: " ready-count "/" total)
        "No players connected."))
    "Status is only available in the lobby."))

;; HELP

(defn help []
  (str "Команды:\n"
       (str/join "\n" (map (fn [[k d]] (str k " — " d)) command-help))
       "\nКороткие ходы: w/a/s/d или стрелки. Паззлы: быстрее = больше XP."))

;; COMMAND MAP

(def commands
  {"move" move
  "north" (fn [] (move :north))
  "south" (fn [] (move :south))
  "east"  (fn [] (move :east))
  "west"  (fn [] (move :west))
   "w"     (fn [] (move :north))
   "s"     (fn [] (move :south))
   "d"     (fn [] (move :east))
   "a"     (fn [] (move :west))
   "grab" grab
   "discard" discard
   "inventory" inventory
   "attack" attack-mob
   "use" use-item
   "equip" equip-item
   "stats" show-stats
   "detect" detect
   "look" look
   "say" say
   "help" help
   "status" lobby-status
   "solve" solve-puzzle})

;; EXECUTE

(defn execute [input]
  (try
    (let [[cmd & args] (.split input " +")]
      (apply (commands cmd) args))
    (catch Exception _
      "You can't do that!")))
