(ns stardust.models
  (:require [stardust.utils :as u]))

(defrecord ObjectPiece [x y vX vY size rotate rotation rotation-factor color lifespan time-left path])

(defrecord Player [client-id x y vX vY h thrust turn accelerate shoot time-before-shot immunity color])

(defrecord Ship [x y h immunity color])

(defrecord DeathMatch [players effects])

(defrecord DeathMatchScreen [out-channel player ships effects])

(defrecord ConnectionScreen [out-channel])

;;
;; Helpers
;;

(defn random-direction
  []
  (if (< (#+clj java.lang.Math/random #+cljs Math/random) 0.5) -1 1))

(defn random-rotation
  []
  (let [direction (#+clj java.lang.Math/random #+cljs Math/random)]
    (cond
     (< direction 0.33) :left
     (< direction 0.66) :right
     :else              :none)))

;;
;; Factory functions
;;

(defn ship-piece
  [x y vX vY rotation color path]
  (let [lifespan (u/random-float 0.4 1.0)]
    (ObjectPiece. x y
                  (+ (/ vX 2) (* (random-direction) (u/random-int 10 60)))
                  (+ (/ vY 2) (* (random-direction) (u/random-int 10 60)))
                  1
                  (random-rotation) rotation (u/random-int 20 50)
                  color
                  lifespan
                  lifespan
                  path)))

(defn player
  [client-id x y immunity color]
  (Player. client-id x y 0 0 0 0 :none false false 0 immunity color))

(defn player-to-ship
  [player]
  (map->Ship player))

(defn connection-screen
  [out-channel]
  (ConnectionScreen. out-channel))

(defn death-match
  []
  (DeathMatch. {} []))

(defn death-match-to-screen
  [{:keys [players effects] :as state} client-id]
  (let [player  (get players client-id)
        ships   (mapv (fn [[k v]] (player-to-ship v)) (dissoc players client-id))]
    (DeathMatchScreen. nil player ships effects)))
