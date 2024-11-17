(ns linear)

(defn operation [f & v]
  (apply mapv f v))
(def v+ (partial operation +))
(def v- (partial operation -))
(def v* (partial operation *))
(def vd (partial operation /))
(defn dot [& v]
  (if (empty? v)
    0
    (apply + (apply v* v))))
(defn v*s [v & s]
  (mapv #(apply * % s) v))
(def m+ (partial operation v+))
(def m- (partial operation v-))
(def m* (partial operation v*))
(def md (partial operation vd))
(defn m*s [m & s]
  (mapv #(v*s % (apply * s)) m))
(defn transpose [m]
  (apply mapv vector m))
(defn m*v [m v]
  (mapv #(dot % v) m))
(defn m*m [& m]
  (reduce
    (fn [m1 m2]
      (operation #(m*v (transpose m2) %) m1))
    m))