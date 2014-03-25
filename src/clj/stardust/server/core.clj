(ns stardust.server.core
  (:require [clojure.core.async :refer [alts! chan close! go go-loop <! >! put! timeout]]
            [stardust.server.handlers :as h]
            [stardust.models :as m]
            [stardust.protocols :as p]
            [stardust.tick :as t]))

;;
;; Game Process
;;

(defn advance-state
  [state commands multiplier]
  (-> state
      (assoc :effects []) ;; TODO: remove?
      (h/handle-commands commands)
      (p/tick multiplier)))

(defn create-events-emmiter
  [events-channel commands-channel]
  (go-loop [state         (m/death-match)
            commands      []
            advance-timer (timeout 20)
            moment        (System/currentTimeMillis)]
           (let [[command ch] (alts! [advance-timer commands-channel])]
             (condp = ch
               commands-channel (when command
                                  (recur state (conj commands command) advance-timer  moment))
               advance-timer    (let [current    (System/currentTimeMillis)
                                      multiplier (/ (- current moment) 1000.0)
                                      new-state  (advance-state state commands multiplier)]
                                  (doseq [event (:events new-state)]
                                    (>! events-channel event))
                                  (recur (assoc new-state :events []) [] (timeout 20) current))))))

(defn create-client-notifier
  [events-channel clients]
  (go-loop [event (<! events-channel)]
           (when event
             (let [[source target data] event
                   active-clients       @clients
                   message              [source data]]
               (if (= target :all)
                 ;; notify everyone
                 (doseq [[client-id client] active-clients]
                   (>! (:out client) message))
                 ;; send privately
                 (when-let [chan (get-in active-clients [target :out])]
                   (>! chan message))))
             (recur (<! events-channel)))))

(defn create-game
  []
  (let [clients          (atom {})
        events-channel   (chan)
        commands-channel (chan)]
    {:clients          clients
     :events-channel   events-channel
     :commands-channel commands-channel
     :events-emmiter   (create-events-emmiter events-channel commands-channel)
     :client-notifier  (create-client-notifier events-channel clients)}))

;;
;;
;;

(defn enter-game
  [{:keys [clients commands-channel]} {:keys [client-id in] :as client}]
  (let [client (assoc client :in-process
                      (go
                       (loop [command (<! in)]
                         (when-not (nil? command)
                           (do
                             (>! commands-channel [client-id command])
                             (recur (<! in)))))
                       (>! commands-channel [client-id [:leave client-id]])
                       (swap! clients dissoc client-id)))]
    (put! in [:enter client-id])
    (swap! clients assoc client-id client)))

(defn find-available-game
  [games]
  (first (filter #(< (count @(:clients %)) 5) games)))

(defn join-game
  [games client]
  (if-let [game (find-available-game games)]
    (do
      (enter-game game client)
      games)
    (let [game (create-game)]
      (enter-game game client)
      (conj games game))))

(defn- destroy-game-if-empty
  [games {:keys [clients commands-channel events-channel] :as game}]
  (if (pos? (count @clients))
    (conj games game)
    (do
      (close! commands-channel)
      (close! events-channel)
      games)))

(defn remove-empty-games
  [games]
  (reduce destroy-game-if-empty [] games))

(defn engine-process
  [connections-channel]
  (go-loop [games []]
           (let [client (<! connections-channel)]
             (recur (-> games
                        (remove-empty-games)
                        (join-game client))))))
