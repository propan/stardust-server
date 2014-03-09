(ns stardust.utils)

(defn sin
  [x]
  (#+clj java.lang.Math/sin #+cljs Math/sin x))

(defn cos
  [x]
  (#+clj java.lang.Math/cos #+cljs Math/cos x))
