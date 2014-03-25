(ns stardust.server.handlers
  (:require [stardust.constants :as C]
            [stardust.models :as m]
            [stardust.protocols :refer [Handler handle]])
  (:import [stardust.models DeathMatch]))

;;
;; Game Screen
;;

(defn- next-player-color
  [players]
  (let [colors-in-game (set (map #(:color (second %)) players))]
    (first (filter #(not (contains? colors-in-game %)) (range 0 5)))))

(defn- handle-enter-event
  [state client-id]
  (let [color  (next-player-color (:players state))
        player (m/player client-id (/ C/FIELD_WIDTH 2) (/ C/FIELD_HEIGHT 2) C/SPAWN_IMMUNITY_SECONDS color)]
    (-> state
        (assoc-in  [:players client-id] player)
        (assoc-in  [:score client-id] 0)
        (update-in [:events] into [[:state client-id (m/death-match-to-screen client-id state)] [:join :all player]]))))

(defn- handle-leave-event
  [state client-id]
  (-> state
      (update-in [:players] dissoc client-id)
      (update-in [:score]   dissoc client-id)
      (update-in [:events]  conj   [:leave :all client-id])))

(defn- emmit-player-state-event
  [state client-id]
  (let [player (get-in state [:players client-id])]
    (update-in state [:events] conj [:player :all player])))

(defn- change-player-state
  [state client-id property from to]
  (let [current (get-in state [:players client-id property])]
    (if (= current from)
      (-> state
          (assoc-in  [:players client-id property] to)
          (emmit-player-state-event client-id))
      state)))

(defn- handle-keyboard-event
  [state client-id event]
  (case event
    :arrow-left-down  (change-player-state state client-id :turn :none  :left)
    :arrow-left-up    (change-player-state state client-id :turn :left  :none)
    :arrow-right-down (change-player-state state client-id :turn :none  :right)
    :arrow-right-up   (change-player-state state client-id :turn :right :none)
    :arrow-up-down    (change-player-state state client-id :accelerate false true)
    :arrow-up-up      (change-player-state state client-id :accelerate true false)
    :space-down       (change-player-state state client-id :shoot false true)
    :space-up         (change-player-state state client-id :shoot true false)
    state))

(extend-type DeathMatch
  Handler
  (handle [state [client-id [source data]]]
    (case source
      :enter    (handle-enter-event state client-id)
      :leave    (handle-leave-event state client-id)
      :keyboard (handle-keyboard-event state client-id data)
      state)))

(defn handle-commands
  [state commands]
  (reduce handle state commands))
