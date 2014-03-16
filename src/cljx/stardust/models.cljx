(ns stardust.models
  (:require [stardust.constants :as C]
            [stardust.utils :as u]))

(defrecord Bullet [cid x y vX vY h e])

(defrecord ObjectPiece [x y vX vY size rotate rotation rotation-factor color lifespan time-left path])

(defrecord Player [client-id x y vX vY h thrust turn accelerate shoot time-before-shot immunity color])

(defrecord Ship [x y h immunity color])

(defrecord DeathMatch [players effects bullets])

(defrecord DeathMatchScreen [out-channel player ships effects bullets])

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

(defn bullet
  [client-id x y heading]
  (let [vX (* C/BULLET_VELOCITY (Math/sin (* heading (- C/RAD_FACTOR))))
        vY (* C/BULLET_VELOCITY (Math/cos (* heading (- C/RAD_FACTOR))))]
    (Bullet. client-id (- x (* 0.05 vX)) (- y (* 0.05 vY)) vX vY heading C/INITIAL_BULLET_ENERGY)))

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
  [{:keys [x y h immunity color]}]
  (Ship. x y h immunity color))

(defn connection-screen
  [out-channel]
  (ConnectionScreen. out-channel))

(defn death-match
  []
  (DeathMatch. {} [] []))

(defn death-match-to-screen
  [{:keys [players effects bullets] :as state} client-id]
  (let [player  (get players client-id)
        ships   (mapv (fn [[k v]] (player-to-ship v)) (dissoc players client-id))]
    (DeathMatchScreen. nil player ships effects bullets)))
