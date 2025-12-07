(ns mire.rooms
  (:require [mire.mobs :as mobs]
            [mire.puzzles :as puzzles]))

(def rooms (ref {}))
(def ^:dynamic *enable-random-content* true)
(declare mark-puzzle-attempted!)

(defn load-room [rooms file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (conj rooms
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc room)
            :exits (ref (:exits room))
            :items (ref (or (:items room) #{}))
            :inhabitants (ref #{})
            :mobs (ref #{})
            :puzzle (ref nil)
            :puzzle-solved-by (ref #{})
            :puzzle-timers (ref {})}})))

(defn load-rooms [rooms dir]
  (dosync
   (reduce load-room rooms (.listFiles (java.io.File. dir)))))

(defn add-rooms [dir]
  (dosync (alter rooms load-rooms dir))
  (dosync
   (doseq [[k room] @rooms]
     (when-not (#{:lobby :start} k)
       (when *enable-random-content*
         (when (< (rand) 0.5)
           (mobs/add-mob-to-room (rand-nth (keys mobs/mobs)) room))
         (when (< (rand) 0.2)
           (alter (:items room) conj :puzzle)
           (ref-set (:puzzle room) (puzzles/random-puzzle))
           (ref-set (:puzzle-solved-by room) #{})
           (ref-set (:puzzle-timers room) {}))
         (when (< (rand) 0.1)
           (alter (:items room) conj :trader)))))))

(defn room-contains? [room thing]
  (@(:items room) (keyword thing)))

(defn puzzle-present? [room]
  (and room @(:puzzle room)))

(defn puzzle-available-for? [room player-name]
  (let [{:keys [deadline]} (get @(:puzzle-timers room) player-name)
        expired? (and deadline (<= deadline (System/currentTimeMillis)))]
    (when expired?
      (mark-puzzle-attempted! room player-name))
    (and (puzzle-present? room)
         (not (@(:puzzle-solved-by room) player-name))
         (not expired?))))

(defn mark-puzzle-attempted! [room player-name]
  (dosync
   (alter (:puzzle-solved-by room) conj player-name)
   (alter (:puzzle-timers room) dissoc player-name)))

(defn get-puzzle [room]
  @(:puzzle room))

(defn set-puzzle-timer! [room player-name started-ms deadline-ms]
  (dosync
   (alter (:puzzle-timers room) assoc player-name {:started started-ms :deadline deadline-ms})))

(defn puzzle-timer [room player-name]
  (get @(:puzzle-timers room) player-name))
