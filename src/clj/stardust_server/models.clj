(ns stardust-server.models)

(defrecord Ship [client-id x y vX vY thrust rotation rotate accelerate shoot time-before-shot radius immunity color])

(defrecord DeathMatch [ships])

(defn ship
  [client-id x y immunity color]
  (Ship. client-id x y 0 0 0 0 :none false false 0 30 immunity color))

(defn death-match
  []
  (DeathMatch. {}))
