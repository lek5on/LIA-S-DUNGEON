(ns mire.core
  (:require [mire.player :as player]
            [mire.items :as items]
            [mire.mobs :as mobs]))

(defn demo
  "Небольшое демо: создаём игрока, экипируем оружие и сражаемся с мобом." []
  (let [stats (player/init-stats 100 2)
        _ (println "Характеристики игрока заданы:" @stats)
        sword (items/get-weapon :sword)
        _ (println "Экипируем меч...")
        _ (player/equip-weapon! stats sword)
        _ (println "Характеристики после экипировки:" @stats)
        mob (mobs/spawn :zombie)]
    (println "Неожиданно появляется" (:name @mob) "!")
    (println (mobs/player-attack-mob! stats mob))
    (println "Состояние моба:" @mob)
    (println (mobs/mob-attack! mob stats))
    (println "Состояние игрока:" @stats)
    (println "Используем малое зелье здоровья...")
    (println (items/use-potion! stats :hp-small))
    (println "Состояние игрока после зелья:" @stats)
    ))

(defn -main [& args]
  (demo))
