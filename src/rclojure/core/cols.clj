(ns rclojure.core.cols)

(defn rvec
  "Takes a clojure sequential coll
   and returns an R vector
   representation to be evaluated"
  ([coll]
   (str "c(" (reduce str (interpose "," coll)) ")")))