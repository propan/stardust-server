(ns stardust.server
  (:require [stardust.webbit :refer [start-server]]
            [stardust.server.core :refer [engine-process]]
            [clojure.core.async :refer [chan go go-loop >! <! close! put!]])
  (:gen-class :main true))

(def next-id!
  (let [counter (java.util.concurrent.atomic.AtomicLong.)]
    (fn []
      (.incrementAndGet counter))))

(defn create-client
  [connection]
  (let [in        (chan)
        out       (chan)
        client-id (next-id!)]
    {:client-id   client-id
     :in          in
     :out         out
     :out-process (go-loop [event (<! out)]
                           (when-not (nil? event)
                             (do
                               (.send connection (pr-str event))
                               (recur (<! out)))))}))

(defn connect-client
  [clients connection connections-channel]
  (let [client (create-client connection)]
    (put! connections-channel client)
    (assoc clients connection client)))

(defn disconnect-client
  [clients connection]
  (if-let [{:keys [in out]} (get clients connection)]
    (do
      (close! out)
      (close! in)
      (dissoc clients connection))
    clients))

(defn handle-message
  [clients {:keys [connection message]}]
  (when-let [{:keys [in out]} (get clients connection)]
    (put! in message))
  clients)

(defn connections-process
  [events]
  (let [connections-channel (chan)]
    (go-loop [clients {}]
             (let [[type data] (<! events)]
               (recur
                (case type
                  :connected    (connect-client clients data connections-channel)
                  :disconnected (disconnect-client clients data)
                  :message      (handle-message clients data)))))
    connections-channel))

(defn -main
  []
  (let [events      (chan)
        connections (connections-process events)]
    (engine-process connections)
    (start-server 8080 events)))
