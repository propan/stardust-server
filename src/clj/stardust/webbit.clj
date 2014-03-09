(ns stardust.webbit
  (:import [org.webbitserver WebServer WebServers WebSocketHandler])
  (:require [clojure.core.async :refer [chan dropping-buffer put!]]))

(defn- safe-read-string
  [str]
  (try
    (read-string str)
    (catch Exception e nil)))

(defn start-server
  [port events]
  (doto (WebServers/createWebServer port)
    (.add "/" (proxy [WebSocketHandler] []
                (onOpen [connection]
                  (put! events [:connected connection]))
                (onClose [connection]
                  (put! events [:disconnected connection]))
                (onMessage [connection data]
                  (if-let [message (safe-read-string data)]
                    (put! events [:message {:connection connection :message message}])))))
    (.start)))
