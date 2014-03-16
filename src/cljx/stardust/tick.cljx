(ns stardust.tick
  (:require [stardust.constants :as C]
            [stardust.models :as m]
            [stardust.protocols :refer [Tickable tick]]
            [stardust.utils :as u]))
;;
;; Effects
;;

(defn create-ship-explosion-effect
  [{:keys [x y vX vY h color] :as ship}]
  (let [points [[-10 10] [0 -15] [10 10] [7 5] [-7 5]]
        pieces (partition 2 1 (take 1 points) points)]
    (map #(m/ship-piece x y vX vY h color %) pieces)))

;;
;; Ship Movement Functions
;;

(defn- next-position
  [position dfn velocity multiplier max-position]
  (let [next (dfn position (* velocity multiplier))]
    (cond
     (>= next max-position) 0
     (< next 0)             (- max-position 1)
     :default               next)))

(defn- next-rotation
  [rotate rotation multiplier rotation-factor]
  (case rotate
    :left    (mod (- rotation (* rotation-factor multiplier)) 360)
    :right   (mod (+ rotation (* rotation-factor multiplier)) 360)
    rotation))

(defn- next-thrust
  [accelerate thrust multiplier]
  (if accelerate
    (min (+ thrust (* multiplier C/ACCELERATION)) C/MAX_THRUST)
    (max 0 (- thrust (* multiplier C/THRUST_DECLINE)))))

(defn- next-velocity
  [velocity accelerate vfn heading thrust multiplier]
  (if accelerate
    (let [next-velocity (+ velocity (* thrust (vfn (* heading C/RAD_FACTOR)) multiplier))]
      (min (max next-velocity (- C/MAX_VELOCITY)) C/MAX_VELOCITY))
    velocity))

(extend-type stardust.models.ObjectPiece
  Tickable
  (tick [{:keys [x y vX vY rotate rotation rotation-factor time-left] :as piece} multiplier]
    (merge piece {:x         (next-position x + vX multiplier C/FIELD_WIDTH)
                  :y         (next-position y + vY multiplier C/FIELD_HEIGHT)
                  :rotation  (next-rotation rotate rotation multiplier rotation-factor)
                  :time-left (- time-left multiplier)})))

(extend-type stardust.models.Player
  Tickable
  (tick [{:keys [x y vX vY h thrust turn accelerate shoot time-before-shot immunity] :as ship} multiplier]
    (let [shoot? (and shoot (zero? time-before-shot))]
      (merge ship {:x                (next-position x + vX multiplier C/FIELD_WIDTH)
                   :y                (next-position y - vY multiplier C/FIELD_HEIGHT)
                   :vX               (next-velocity vX accelerate u/sin h thrust multiplier)
                   :vY               (next-velocity vY accelerate u/cos h thrust multiplier)
                   :h                (next-rotation turn h multiplier C/TURN_FACTOR)
                   :thrust           (next-thrust accelerate thrust multiplier)
                   :time-before-shot (if shoot?
                                       C/SECONDS_BETWEEN_SHOOTS
                                       (max 0 (- time-before-shot multiplier)))
                   :immunity         (max 0 (- immunity multiplier))}))))

(defn- pair-players
  [players]
  (let [px (filter #(zero? (:immunity %)) (vals players))]
    (for [p1 px p2 px :when (< (:client-id p1) (:client-id p2))]
      [p1 p2])))

(defn- ships-collide?
  [s1 s2]
  (<= (u/distance (:x s1) (:y s1) (:x s2) (:y s2))
      (* 2 C/SHIP_RADIUS)))

(defn- detect-players-collisions
  [{:keys [players effects] :as state}]
  (loop [players     players
         new-effects {}
         pairs       (pair-players players)]
    (if-let [[p1 p2] (first pairs)]
      (if (ships-collide? p1 p2)
        (let [client-id-1 (:client-id p1)
              client-id-2 (:client-id p2)]
          ;; TODO: refactor?
          (recur (assoc players
                   client-id-1 (m/player client-id-1 (/ C/FIELD_WIDTH 2) (/ C/FIELD_HEIGHT 2) C/SPAWN_IMMUNITY_SECONDS (:color p1))
                   client-id-2 (m/player client-id-2 (/ C/FIELD_WIDTH 2) (/ C/FIELD_HEIGHT 2) C/SPAWN_IMMUNITY_SECONDS (:color p2)))
                 (assoc new-effects
                   client-id-1 (create-ship-explosion-effect p1)
                   client-id-2 (create-ship-explosion-effect p2))
                 (rest pairs)))
        (recur players new-effects (rest pairs)))
      (merge state {:players players
                    :effects (apply concat effects (vals new-effects))}))))

(extend-type stardust.models.DeathMatch
  Tickable
  (tick [{:keys [players] :as state} multiplier]
    (-> state
        (assoc :players (reduce (fn [m [k v]] (assoc m k (tick v multiplier))) {} players))
        (detect-players-collisions))))

(extend-type stardust.models.ConnectionScreen
  Tickable
  (tick [state multiplier]
    state))

(defn- effects-tick
  [effects multiplier]
  (->> effects
       (map #(tick % multiplier) effects)
       (filter #(pos? (:time-left %)))))

(extend-type stardust.models.DeathMatchScreen
  Tickable
  (tick [{:keys [players effects] :as state} multiplier]
    (-> state
        (assoc :effects (effects-tick effects multiplier)))))
