(ns stardust-server.core
  (:require [clojure.core.async :refer [alts! chan go go-loop <! >! put! timeout]]
            [stardust-server.models :as m]))

;;
;; Game Process
;;

(defn create-state-emmiter
  [state-channel events-channel]
  (go-loop [state  (m/death-match 1000 600)
            events []
            timer  (timeout 100)]
           (let [[event ch] (alts! [timer events-channel])]
             (condp = ch
               events-channel (when event
                                (recur state (conj events event) timer))
               timer          (let [new-state (m/handle-events state events)]
                                (>! state-channel new-state)
                                (recur new-state [] (timeout 100)))))))

(defn create-client-notifier
  [state-channel clients]
  (go-loop [state (<! state-channel)]
           (when state
             (doseq [[client-id client] @clients]
               (>! (:out client) (pr-str state)))
             (recur (<! state-channel)))))

(defn create-game
  []
  (let [clients        (atom {})
        state-channel  (chan)
        events-channel (chan)]
    {:clients         clients
     :state-channel   state-channel
     :events-channel  events-channel
     :state-emmiter   (create-state-emmiter state-channel events-channel)
     :client-notifier (create-client-notifier state-channel clients)}))

;;
;;
;;

(defn enter-game
  [{:keys [clients events-channel]} {:keys [client-id in] :as client}]
  (let [client (assoc client :in-process
                      (go
                       (loop [event (<! in)]
                         (when-not (nil? event)
                           (do
                             (>! events-channel [client-id event])
                             (recur (<! in)))))
                       (>! events-channel [client-id [:leave client-id]])
                       (swap! clients dissoc client-id)))]
    (put! in [:enter client-id])
    (swap! clients assoc client-id client)))

(defn find-available-game
  [games]
  (first (filter #(< (count @(:clients %)) 6) games)))

(defn join-game
  [games client]
  (if-let [game (find-available-game games)]
    (do
      (enter-game game client)
      games)
    (let [game (create-game)]
      (enter-game game client)
      (conj games game))))

;; TODO: GC empty games

(defn engine-process
  [connections-channel]
  (go-loop [games []]
           (let [client (<! connections-channel)]
             (recur (join-game games client)))))
