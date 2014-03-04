(defproject stardust-server "0.1.0-SNAPSHOT"
  :description "the back-end of Stardust game"
  :url "http://github.com/prokpa/stardust-server"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.webbitserver/webbit "0.4.15"]]
  :main stardust-server.server/-main)
