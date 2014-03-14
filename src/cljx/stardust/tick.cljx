(ns stardust.tick
  (:require [stardust.constants :as C]
            [stardust.protocols :refer [Tickable tick]]
            [stardust.utils :as u]))

;;
;; Ship Movement Functions
;;

(defn- next-position
  [position dFn velocity multiplier max-position]
  (let [next (dFn position (* velocity multiplier))]
    (cond
     (>= next max-position) 0
     (< next 0)             (- max-position 1)
     :default               next)))

(defn- next-rotation
  [rotate rotation multiplier turn-factor]
  (case rotate
    :left    (mod (- rotation (* turn-factor multiplier)) 360)
    :right   (mod (+ rotation (* turn-factor multiplier)) 360)
    rotation))

(defn- next-thrust
  [accelerate thrust multiplier]
  (if accelerate
    (min (+ thrust (* multiplier C/ACCELERATION)) C/MAX_THRUST)
    (max 0 (- thrust (* multiplier C/THRUST_DECLINE)))))

(defn- next-velocity
  [vFn velocity accelerate rotation thrust multiplier]
  (if accelerate
    (let [next-velocity (+ velocity (* thrust (vFn (* rotation C/RAD_FACTOR)) multiplier))]
      (min (max next-velocity (- C/MAX_VELOCITY)) C/MAX_VELOCITY))
    velocity))

(extend-type stardust.models.Player
  Tickable
  (tick [{:keys [x y vX vY rotation thrust accelerate rotate shoot time-before-shot immunity] :as ship} multiplier]
    (let [shoot? (and shoot (zero? time-before-shot))]
      (merge ship {:x                (next-position x + vX multiplier C/FIELD_WIDTH)
                   :y                (next-position y - vY multiplier C/FIELD_HEIGHT)
                   :vX               (next-velocity u/sin vX accelerate rotation thrust multiplier)
                   :vY               (next-velocity u/cos vY accelerate rotation thrust multiplier)
                   :rotation         (next-rotation rotate rotation multiplier C/TURN_FACTOR)
                   :thrust           (next-thrust accelerate thrust multiplier)
                   :time-before-shot (if shoot?
                                       C/SECONDS_BETWEEN_SHOOTS
                                       (max 0 (- time-before-shot multiplier)))
                   :immunity         (max 0 (- immunity multiplier))}))))

(extend-type stardust.models.DeathMatch
  Tickable
  (tick [{:keys [players] :as state} multiplier]
    (assoc state :players (reduce (fn [m [k v]] (assoc m k (tick v multiplier))) {} players))))
