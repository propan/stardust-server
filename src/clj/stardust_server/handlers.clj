(ns stardust-server.handlers
  (:require [stardust-server.constants :as C]
            [stardust-server.models :as m])
  (:import [stardust_server.models DeathMatch]))


;;
;; Handler Protocol
;;

(defprotocol Handler
  (handle [_ event]))

;;
;; Game Screen
;;

(defn- next-ship-color
  [ships]
  (let [colors-in-game (set (map #(:color (second %)) ships))]
    (first (filter #(not (contains? colors-in-game %)) (range 1 6)))))

(defn- handle-enter-event
  [state client-id]
  (update-in state [:ships]
             (fn [ships]
               (let [color (next-ship-color ships)]
                 (assoc ships client-id
                        (m/ship client-id (/ C/FIELD_WIDTH 2) (/ C/FIELD_HEIGHT 2) C/SPAWN_IMMUNITY_SECONDS color))))))

(defn- handle-leave-event
  [state client-id]
  (update-in state [:ships] dissoc client-id))

(defn- change-player-state
  [state client-id property from to]
  (update-in state [:ships client-id property] #(if (= % from) to %)))

(defn- handle-keyboard-event
  [state client-id event]
  (case event
    :arrow-left-down  (change-player-state state client-id :rotate :none :left)
    :arrow-left-up    (change-player-state state client-id :rotate :left :none)
    :arrow-right-down (change-player-state state client-id :rotate :none :right)
    :arrow-right-up   (change-player-state state client-id :rotate :right :none)
    :arrow-up-down    (change-player-state state client-id :accelerate false true)
    :arrow-up-up      (change-player-state state client-id :accelerate true false)
    state))

(extend-type DeathMatch
  Handler
  (handle [state [client-id [source data]]]
    (case source
      :enter    (handle-enter-event state client-id)
      :leave    (handle-leave-event state client-id)
      :keyboard (handle-keyboard-event state client-id data)
      state)))

(defn handle-events
  [state events]
  (reduce handle state events))
