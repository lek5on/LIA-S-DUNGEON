(ns mire.mobs
  (:require [mire.player :as player]))

;; Определения мобов и простые вспомогательные функции боя.

(def mobs
  {:zombie {:name "Зомби" :hp 30 :damage 5 :xp 20}
   :skeleton {:name "Скелет" :hp 25 :damage 6 :xp 25}
   :witch {:name "Ведьма" :hp 40 :damage 8 :xp 40}
   :slenderman {:name "Слендермен" :hp 80 :damage 12 :xp 120}})

(defn spawn
  "Создать экземпляр моба из справочника." [k]
  (when-let [m (get mobs (keyword k))]
    (atom (assoc m :id (keyword k) :hp (:hp m)))))

(defn add-mob-to-room
  "Создать моба с ключом `k` и добавить в ссылку :mobs комнаты.
   `room` должна быть картой комнаты. Возвращает атом моба." [k room]
  (let [mob-atom (spawn k)]
    (dosync
      (alter (:mobs room) conj mob-atom))
    mob-atom))

(defn remove-mob-from-room!
  "Удалить моб из ссылки :mobs комнаты. Нужно вызывать в транзакции или через dosync.
   Возвращает true, если моб удалён." [mob-atom room]
  (dosync
   (alter (:mobs room) disj mob-atom)))

(defn find-mob-in-room
  "Найти атом моба в комнате по keyword-ид или вернуть любого, если id nil." [room id]
  (let [all @(:mobs room)]
    (if id
      (first (filter #(= (:id @%) (keyword id)) all))
      (first all))))

(defn mob-attack!
  "Моб атакует характеристики игрока."
  [mob-atom player-stats-ref]
  (let [dmg (:damage @mob-atom 0)]
    (player/damage! player-stats-ref dmg)
    (str (:name @mob-atom) " наносит вам " dmg " урона.")))

(defn player-attack-mob!
  "Игрок (stats-ref) атакует моба. Возвращает сообщение и наносит урон мобу." [player-stats mob-atom]
  (let [pdmg (:damage @player-stats)]
    (swap! mob-atom update :hp (fn [h] (max 0 (- h pdmg))))
    (str "Вы ударили " (:name @mob-atom) " на " pdmg " урона.")))
