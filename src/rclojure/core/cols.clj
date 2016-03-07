(ns rclojure.core.cols)

;TODO ammend rvec to use pretty print

(defn rvec
  "Takes a clojure collection of objects
   and returnsan an array of primitives."
  ([coll]
   (double-array coll))
  ([coll {:keys [ints? doubles? print?]}]
    (cond (= ints? true)
          (int-array coll)
          (= doubles? true)
          (double-array coll)
          (and (= ints? true) (= print? true))
          (for [x (int-array coll)]
            (println x)))))