(ns stardust.webbit
  (:import [org.webbitserver WebServer WebServers WebSocketHandler]
           [org.webbitserver.netty NettyWebSocketConnection])
  (:require [clojure.core.async :refer [chan dropping-buffer put!]]
            [taoensso.timbre :as log]))

(defn- safe-read-string
  [str]
  (try
    (read-string str)
    (catch Exception e nil)))

(defn remote-addr
  [^NettyWebSocketConnection connection]
  (try
    (-> connection
        .httpRequest
        .remoteAddress
        .getAddress
        .getHostAddress)
    (catch Exception e "n/a")))

(defn start-server
  [port events]
  (doto (WebServers/createWebServer port)
    (.add "/" (proxy [WebSocketHandler] []
                (onOpen [connection]
                  (log/debug "Client connected" (remote-addr connection))
                  (put! events [:connected connection]))
                (onClose [connection]
                  (log/debug "Client disconnected" (remote-addr connection))
                  (put! events [:disconnected connection]))
                (onMessage [connection data]
                  (if-let [message (safe-read-string data)]
                    (put! events [:message {:connection connection :message message}])))))
    (.start)))
