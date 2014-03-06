(ns stardust-server.models)

(defrecord Ship [client-id x y vX vY thrust rotation rotate accelerate shoot ticks-before-shoot radius immunity color])

(defrecord DeathMatch [width height ships])

(defn ship
  [client-id x y immunity color]
  (Ship. client-id x y 0 0 0 0 :none false false 0 30 immunity color))

(defn death-match
  [width height]
  (DeathMatch. width height {}))

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
  [{:keys [width height] :as state} client-id]
  (update-in state [:ships]
             (fn [ships]
               (let [color (next-ship-color ships)]
                 (assoc ships client-id (ship client-id (/ width 2) (/ height 2) 100 color))))))

(defn- handle-leave-event
  [{:keys [width height] :as state} client-id]
  (update-in state [:ships] dissoc client-id))

(extend-type DeathMatch
  Handler
  (handle [state [client-id [source data]]]
    (case source
      :enter (handle-enter-event state client-id)
      :leave (handle-leave-event state client-id)
      state)))

(defn handle-events
  [state events]
  (reduce handle state events))
