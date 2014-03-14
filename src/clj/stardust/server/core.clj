(ns stardust.server.core
  (:require [clojure.core.async :refer [alts! chan go go-loop <! >! put! timeout]]
            [stardust.server.handlers :as h]
            [stardust.models :as m]
            [stardust.protocols :as p]
            [stardust.tick :as t]))

;;
;; Game Process
;;

(defn advance-state
  [state events multiplier]
  (-> state
      (h/handle-events events)
      (p/tick multiplier)))

(defn create-state-emmiter
  [state-channel events-channel]
  (go-loop [state  (m/death-match)
            events []
            timer  (timeout 20)
            moment (System/currentTimeMillis)]
           (let [[event ch] (alts! [timer events-channel])]
             (condp = ch
               events-channel (when event
                                (recur state (conj events event) timer moment))
               timer          (let [current    (System/currentTimeMillis)
                                    multiplier (/ (- current moment) 1000.0)
                                    new-state  (advance-state state events multiplier)]
                                (>! state-channel new-state)
                                (recur new-state [] (timeout 20) current))))))

(defn create-client-notifier
  [state-channel clients]
  (go-loop [state (<! state-channel)]
           (when state
             (doseq [[client-id client] @clients]
               (>! (:out client) (m/death-match-to-screen state client-id)))
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

(defn gc
  [games]
  (filter #(pos? (count @(:clients %))) games))

(defn engine-process
  [connections-channel]
  (go-loop [games []]
           (let [client (<! connections-channel)]
             (recur (gc (join-game games client))))))
