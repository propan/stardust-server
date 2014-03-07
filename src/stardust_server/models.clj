(ns stardust-server.models
  (:require [stardust-server.constants :as C]
            [stardust-server.utils :as u]))

(defrecord Ship [client-id x y vX vY thrust rotation rotate accelerate shoot time-before-shot radius immunity color])

(defrecord DeathMatch [ships])

(defn ship
  [client-id x y immunity color]
  (Ship. client-id x y 0 0 0 0 :none false false 0 30 immunity color))

(defn death-match
  []
  (DeathMatch. {}))

;;
;; Handler Protocol
;;

(defprotocol Handler
  (handle [_ event]))

;;
;; Game Screen
;;

(defn- next-ship-color
  [ships]
  (let [colors-in-game (set (map #(:color (second %)) ships))]
    (first (filter #(not (contains? colors-in-game %)) (range 1 6)))))

(defn- handle-enter-event
  [state client-id]
  (update-in state [:ships]
             (fn [ships]
               (let [color (next-ship-color ships)]
                 (assoc ships client-id
                        (ship client-id (/ C/FIELD_WIDTH 2) (/ C/FIELD_HEIGHT 2) C/SPAWN_IMMUNITY_SECONDS color))))))

(defn- handle-leave-event
  [state client-id]
  (update-in state [:ships] dissoc client-id))

(defn- change-player-state
  [state client-id property from to]
  (update-in state [:ships client-id property] #(if (= % from) to %)))

(defn- handle-keyboard-event
  [state client-id event]
  (case event
    :arrow-left-down  (change-player-state state client-id :rotate :none :left)
    :arrow-left-up    (change-player-state state client-id :rotate :left :none)
    :arrow-right-down (change-player-state state client-id :rotate :none :right)
    :arrow-right-up   (change-player-state state client-id :rotate :right :none)
    :arrow-up-down    (change-player-state state client-id :accelerate false true)
    :arrow-up-up      (change-player-state state client-id :accelerate true false)
    state))

(extend-type DeathMatch
  Handler
  (handle [state [client-id [source data]]]
    (case source
      :enter    (handle-enter-event state client-id)
      :leave    (handle-leave-event state client-id)
      :keyboard (handle-keyboard-event state client-id data)
      state)))

;;
;; Tickable Protocol
;;

(defprotocol Tickable
  (tick [_ multiplier]))

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

(extend-type Ship
  Tickable
  (tick [{:keys [x y vX vY rotation thrust accelerate rotate shoot time-before-shot immunity] :as ship} multiplier]
    (let [shoot? (and shoot (zero? time-before-shot))
          time   (* multiplier 1000)]
      (merge ship {:x                (next-position x + vX multiplier C/FIELD_WIDTH)
                   :y                (next-position y - vY multiplier C/FIELD_HEIGHT)
                   :vX               (next-velocity u/sin vX accelerate rotation thrust multiplier)
                   :vY               (next-velocity u/cos vY accelerate rotation thrust multiplier)
                   :rotation         (next-rotation rotate rotation multiplier C/TURN_FACTOR)
                   :thrust           (next-thrust accelerate thrust multiplier)
                   :time-before-shot (if shoot?
                                       C/SECONDS_BETWEEN_SHOOTS
                                       (max 0 (- time-before-shot time)))
                   :immunity         (max 0 (- immunity time))}))))

(extend-type DeathMatch
  Tickable
  (tick [{:keys [ships] :as state} multiplier]
    (assoc state :ships (reduce (fn [m [k v]] (assoc m k (tick v multiplier))) {} ships))))

;;
;;
;;

(defn handle-events
  [state events]
  (reduce handle state events))

(defn advance-state
  [state events multiplier]
  (-> state
      (handle-events events)
      (tick multiplier)))
