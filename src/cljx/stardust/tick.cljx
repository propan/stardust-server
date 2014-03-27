(ns stardust.tick
  (:require [stardust.constants :as C]
            [stardust.models :as m]
            [stardust.protocols :refer [Tickable tick]]
            [stardust.utils :as u]))

;;
;; Movement Functions
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

(extend-type stardust.models.Bullet
  Tickable
  (tick [{:keys [x y vX vY e] :as bullet} multiplier]
    (merge bullet {:x (next-position x - vX multiplier C/FIELD_WIDTH)
                   :y (next-position y - vY multiplier C/FIELD_HEIGHT)
                   :e (- e (* C/BULLET_ENERGY_DECLINE multiplier))})))

(extend-type stardust.models.ObjectPiece
  Tickable
  (tick [{:keys [x y vX vY rotate rotation rotation-factor time-left] :as piece} multiplier]
    (merge piece {:x         (next-position x + vX multiplier C/FIELD_WIDTH)
                  :y         (next-position y + vY multiplier C/FIELD_HEIGHT)
                  :rotation  (next-rotation rotate rotation multiplier rotation-factor)
                  :time-left (- time-left multiplier)})))

(extend-type stardust.models.Particle
  Tickable
  (tick [{:keys [x y vX vY time-left] :as particle} multiplier]
    (merge particle {:x         (next-position x - vX multiplier C/FIELD_WIDTH)
                     :y         (next-position y - vY multiplier C/FIELD_HEIGHT)
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
      C/COLLISION_DISTANCE))

(defn- respawn-player
  [state client-id]
  (let [color  (get-in state [:players client-id :color])
        player (m/player client-id color)]
    (-> state
        (assoc-in  [:players client-id] player)
        (update-in [:score client-id] dec)
        (update-in [:events] conj [:spawn :all player]))))

(defn- detect-players-collisions
  [{:keys [players] :as state}]
  (loop [pairs      (pair-players players)
         collisions #{}]
    (if-let [[p1 p2] (first pairs)]
      (if (ships-collide? p1 p2)
        (recur (rest pairs) (conj collisions (:client-id p1) (:client-id p2)))
        (recur (rest pairs) collisions))
      (reduce respawn-player state collisions))))

(defn- bullet-hit?
  [client-id x y bullet]
  (and (not (= client-id (:cid bullet)))
       (<= (u/distance x y (:x bullet) (:y bullet))
           (+ C/BULLET_RADIUS C/SHIP_RADIUS))))

(defn- handle-bullets
  [{:keys [client-id x y color life] :as player} bullets events score]
  (loop [life    life
         bullets bullets
         miss    (transient [])
         events  events
         score   score]
    (if-let [bullet (first bullets)]
      (if (bullet-hit? client-id x y bullet)
        (let [left (- life (:e bullet))]
          (if (pos? left)
            (recur left (rest bullets) miss (conj! events [:hit :all bullet]) score)
            (let [player    (m/player client-id color)
                  killer-id (:cid bullet)
                  points    (inc (get score killer-id))]
              [player
               (into bullets (persistent! miss))
               (reduce conj! events [[:hit :all bullet] [:spawn :all player] [:score :all [killer-id points]]])
               (assoc! score killer-id points)])))
        (recur life (rest bullets) (conj! miss bullet) events score))
      [(assoc player :life life) (into bullets (persistent! miss)) events score])))

(defn detect-bullets-hits
  [{:keys [players bullets events score] :as state}]
  (loop [players players
         events  (transient events)
         score   (transient score)
         bullets bullets
         ships   (vals players)]
    (if (and (seq ships)
             (seq bullets))
      (let [ship (first ships)]
        (if-not (pos? (:immunity ship))
          (let [[player bullets events score] (handle-bullets ship bullets events score)]
            (recur (assoc players (:client-id player) player) events score bullets (rest ships)))
          (recur players events score bullets (rest ships))))
      (merge state {:players players
                    :bullets bullets
                    :events  (persistent! events)
                    :score   (persistent! score)}))))

(defn- player-shoot
  [bullets [client-id player]]
  (let [{:keys [client-id x y h shoot time-before-shot]} player]
    (if (and shoot (zero? time-before-shot))
      (cons (m/bullet client-id x y h) bullets)
      bullets)))

(defn- bullets-tick
  [players bullets multiplier]
  (->>
   (reduce player-shoot bullets players)
   (map #(tick % multiplier))
   (filter #(pos? (:e %)))
   (into [])))

(extend-type stardust.models.DeathMatch
  Tickable
  (tick [{:keys [players bullets] :as state} multiplier]
    (-> state
        (merge {:players (reduce (fn [m [k v]] (assoc m k (tick v multiplier))) {} players)
                :bullets (bullets-tick players bullets multiplier)})
        (detect-players-collisions)
        (detect-bullets-hits))))

(extend-type stardust.models.ConnectionScreen
  Tickable
  (tick [state multiplier]
    state))

(defn- effects-tick
  [effects multiplier]
  (->> effects
       (map #(tick % multiplier) effects)
       (filter #(pos? (:time-left %)))
       (into [])))

(extend-type stardust.models.DeathMatchScreen
  Tickable
  (tick [{:keys [players bullets effects] :as state} multiplier]
    (-> state
        (merge {:players (reduce (fn [m [k v]] (assoc m k (tick v multiplier))) {} players)
                :bullets (bullets-tick players bullets multiplier)})
        (assoc :effects (effects-tick effects multiplier)))))
