(ns stardust-server.constants)

(def RAD_FACTOR (/ #+clj java.lang.Math/PI #+cljs Math/PI 180))

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

(def MAX_VELOCITY 30)
(def MAX_THRUST 15)

(def THRUST_DECLINE 10)

(def TURN_FACTOR 180)
(def ACCELERATION 5)

;;
;; Shooting
;;

(def SECONDS_BETWEEN_SHOOTS 0.5)

(def SPAWN_IMMUNITY_SECONDS 3)
