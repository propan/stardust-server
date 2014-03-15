(ns stardust.utils)

(defn random-int
  [min max]
  (#+clj java.lang.Math/floor #+cljs Math/floor (+ min (* (#+clj java.lang.Math/random #+cljs Math/random) (- max min -1)))))

(defn random-float
  [min max]
  (+ min (* (#+clj java.lang.Math/random #+cljs Math/random) (- max min))))

(defn sin
  [x]
  (#+clj java.lang.Math/sin #+cljs Math/sin x))

(defn cos
  [x]
  (#+clj java.lang.Math/cos #+cljs Math/cos x))

(defn round
  [x]
  (/ (#+clj java.lang.Math/ceil #+cljs Math/ceil (* x 100)) 100))

(defn distance
  [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (#+clj java.lang.Math/sqrt #+cljs Math/sqrt (+ (* dx dx) (* dy dy)))))
