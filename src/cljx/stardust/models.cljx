(ns stardust.models)

(defrecord Player [client-id x y vX vY thrust rotation rotate accelerate shoot time-before-shot radius immunity color])

(defrecord Ship [x y rotation radius immunity color])

(defrecord DeathMatch [players])

(defrecord DeathMatchScreen [player ships])

(defn player
  [client-id x y immunity color]
  (Player. client-id x y 0 0 0 0 :none false false 0 30 immunity color))

(defn player-to-ship
  [player]
  (map->Ship player))

(defn death-match
  []
  (DeathMatch. {}))

(defn death-match-to-screen
  [state client-id]
  (let [players (:players state)
        player  (get players client-id)
        ships   (map (fn [k v] (player-to-ship v)) (dissoc players client-id))]
    (DeathMatchScreen. player ships)))
