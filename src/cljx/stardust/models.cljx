(ns stardust.models
  (:require [stardust.constants :as C]
            [stardust.utils :as u]))

(defrecord Bullet [cid x y vX vY h e])

(defrecord ObjectPiece [x y vX vY size rotate rotation rotation-factor color lifespan time-left path])

(defrecord Particle [x y vX vY h s lifespan time-left])

(defrecord Player [client-id x y vX vY h thrust turn accelerate shoot time-before-shot immunity color life])

(defrecord Ship [x y h immunity color])

(defrecord DeathMatch [players effects bullets score])

(defrecord DeathMatchScreen [out-channel player ships effects bullets score])

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
  (let [vX (* C/BULLET_VELOCITY (u/sin (* heading (- C/RAD_FACTOR))))
        vY (* C/BULLET_VELOCITY (u/cos (* heading (- C/RAD_FACTOR))))]
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

(defn particle
  [x y]
  (let [heading  (u/random-int 0 360)
        velocity (u/random-float C/MIN_PARTICLE_SPEED C/MAX_PARTICLE_SPEED)
        vX       (* velocity (u/sin (* heading (- C/RAD_FACTOR))))
        vY       (* velocity (u/cos (* heading (- C/RAD_FACTOR))))
        lifespan (u/random-float 0.2 0.4)]
    (Particle. x y vX vY heading (u/random-int 1 2) lifespan lifespan)))

(defn player
  [client-id x y immunity color]
  (Player. client-id x y 0 0 0 0 :none false false 0 immunity color C/MAX_PLAYER_LIFE))

(defn player-to-ship
  [{:keys [x y h immunity color]}]
  (Ship. x y h immunity color))

(defn connection-screen
  [out-channel]
  (ConnectionScreen. out-channel))

(defn death-match
  []
  (DeathMatch. {} [] [] {}))

(defn death-match-to-screen
  [{:keys [players effects bullets score] :as state} client-id]
  (let [player  (get players client-id)
        ships   (reduce (fn [sx [k v]] (assoc sx k (player-to-ship v))) {} (dissoc players client-id))]
    (DeathMatchScreen. nil player ships effects bullets score)))
