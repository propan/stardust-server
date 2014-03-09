(ns stardust.utils)

(defn sin
  [x]
  (#+clj java.lang.Math/sin #+cljs Math/sin x))

(defn cos
  [x]
  (#+clj java.lang.Math/cos #+cljs Math/cos x))

(defn round
  [x]
  (/ (#+clj java.lang.Math/ceil #+cljs Math/ceil (* x 100)) 100))
