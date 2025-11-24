(ns mire.rooms
  (:require [mire.mobs :as mobs]
            [mire.puzzles :as puzzles]))

(def rooms (ref {}))

(defn load-room [rooms file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (conj rooms
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc room)
            :exits (ref (:exits room))
            :items (ref (or (:items room) #{}))
            :inhabitants (ref #{})
            :mobs (ref #{})}})))

(defn load-rooms
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing room data."
  [rooms dir]
  (dosync
   (reduce load-room rooms
           (.listFiles (java.io.File. dir)))))

(defn add-rooms
  "Look through all the files in a dir for files describing rooms and add
  them to the mire.rooms/rooms map."
  [dir]
  (dosync
   (alter rooms load-rooms dir))
  ;; After loading rooms, randomly populate non-start rooms with mobs/puzzles/traders
  (dosync
   (doseq [[k room] @rooms]
     (when (not= k :start)
       (let [r @(:items room)]
         ;; 50% chance to add a mob
         (when (< (rand) 0.5)
           (mobs/add-mob-to-room (rand-nth (keys mobs/mobs)) room))
         ;; 20% chance to set a puzzle
         (when (< (rand) 0.2)
           (alter (:items room) conj :puzzle)
           (dosync (alter (:items room) identity)))
         ;; 10% chance to add a trader flag (represented as an item :trader)
         (when (< (rand) 0.1)
           (alter (:items room) conj :trader)))))))

(defn room-contains?
  [room thing]
  (@(:items room) (keyword thing)))
