(ns mire.core
  (:require [mire.player :as player]
            [mire.items :as items]
            [mire.mobs :as mobs]))

(defn demo []
  (let [stats (player/init-stats 100 2)
        _ (println "Player stats initialized:" @stats)
        sword (items/get-weapon :sword)
        _ (println "Equipping sword...")
        _ (player/equip-weapon! stats sword)
        _ (println "Stats after equipping:" @stats)
        mob (mobs/spawn :zombie)]
    (println "A wild" (:name @mob) "appears!")
    (println (mobs/player-attack-mob! stats mob))
    (println "Mob status:" @mob)
    (println (mobs/mob-attack! mob stats))
    (println "Player status:" @stats)
    (println "Using small potion...")
    (println (items/use-potion! stats :hp-small))
    (println "Player status after potion:" @stats)))

(defn -main [& args]
  (demo))
