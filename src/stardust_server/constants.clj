(ns stardust-server.constants)

(def RAD_FACTOR (/ java.lang.Math/PI 180))

;;
;; Dimensions
;;

(def FIELD_WIDTH 1000)
(def FIELD_HEIGHT 600)

(def RIGHT_EDGE (- FIELD_WIDTH 5))
(def BOTTOM_EDGE (- FIELD_HEIGHT 5))

;;
;; Movement
;;

(def MAX_VELOCITY 6)
(def MAX_THRUST 2)

(def THRUST_DECLINE 0.3)

(def TURN_FACTOR 4)
(def ACCELERATION 0.01)

;;
;; Shooting
;;

(def TICKS_BETWEEN_SHOOTS 20)
