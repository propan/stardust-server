(ns stardust-server.models)

(defrecord Ship [client-id x y vX vY thrust rotation rotate accelerate shoot ticks-before-shoot radius immunity type])

(defrecord GameScreen [width height ships])

(defn ship
  [client-id x y immunity type]
  (Ship. client-id x y 0 0 0 0 :none false false 0 30 immunity type))

(defn game-screen
  [width height]
  (GameScreen. width height {}))

;;
;; Handler Protocol
;;

(defprotocol Handler
  (handle [_ event]))

;;
;; Game Screen
;;

(defn handle-enter-event
  [{:keys [width height] :as state} client-id]
  (assoc-in state [:ships client-id] (ship client-id (/ width 2) (/ height 2) 100 1))) ;; TODO: choose type!

(defn handle-leave-event
  [{:keys [width height] :as state} client-id]
  (update-in state [:ships] dissoc client-id))

(extend-type GameScreen
  Handler
  (handle [state [client-id [source data]]]
    (case source
      :enter (handle-enter-event state client-id)
      :leave (handle-leave-event state client-id)
      state)))

(defn handle-events
  [state events]
  (reduce handle state events))
