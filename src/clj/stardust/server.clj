(ns stardust.server
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :refer [chan go go-loop >! <! close! put!]]
            [stardust.webbit :refer [start-server]]
            [stardust.server.core :refer [engine-process]]
            [clojure.string :as string]
            [taoensso.timbre :as log])
  (:gen-class :main true))

(def options
  [["-p" "--port PORT" "A port to run the server on"
    :default 8080
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-c" "--console" "A flag that enables output to console" :default false]
   ["-l" "--logging LEVEL" "A logging level to use"
    :default 4
    :parse-fn #(Integer/parseInt %)]])

(def logging-levels
  {5 :trace 4 :debug 3 :info 2 :warn 1 :error 0 :fatal})

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
     :out-process (go
                   (loop [event (<! out)]
                     (when-not (nil? event)
                       (do
                         (.send connection (pr-str event))
                         (recur (<! out)))))
                   (log/debug "Stopping client #" client-id "output process"))}))

(defn connect-client
  [clients connection connections-channel]
  (let [client (create-client connection)]
    (put! connections-channel client)
    (assoc clients connection client)))

(defn disconnect-client
  [clients connection]
  (if-let [{:keys [client-id in out]} (get clients connection)]
    (do
      (log/debug "Removing client #" client-id "from connected clients")
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
  (log/debug "Starting server connections process")
  (let [connections-channel (chan)]
    (go
     (loop [clients {}]
       (let [[type data] (<! events)]
         (recur
          (case type
            :connected    (connect-client clients data connections-channel)
            :disconnected (disconnect-client clients data)
            :message      (handle-message clients data)))))
     (log/debug "Stopping server connections process"))
    connections-channel))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args options)]
    (when errors
      (exit 1 (error-msg errors)))

    (let [{:keys [port console logging]} options
          logging-level                  (get logging-levels logging :debug)]

      (log/set-level! logging-level)
      (log/set-config! [:appenders :standard-out :enabled?] console)
      (log/set-config! [:shared-appender-config :spit-filename] "stardust-server.log")
      (log/set-config! [:appenders :spit :enabled?] true)

      (log/info "Setting loging level to" logging-level)
      (log/info "Starting Stardust server on" port)

      (let [events      (chan)
            connections (connections-process events)]
        (engine-process connections)
        (start-server port events)))))
