(ns stardust.protocols)

(defprotocol Handler
  (handle [_ event]))

(defprotocol Tickable
  (tick [_ multiplier]))

