(ns rclojure.core.coll-expression)

(defn str-combine [coll] (str "c(\""(reduce str (interpose "\",\"" coll))"\")"))

(defn as-vec
  ([coll] (str "as.vector(c(" (reduce str (interpose "," coll))"))"))
  ([coll {:keys [mode]}]
              (cond (= mode :character)
                    (str "c(\"" (reduce str (interpose "\",\"" coll)) "\")")
                    (or (= mode :integer) (= mode :double))
                    (str "as.vector(c(" (reduce str (interpose "," coll)) "), mode = \"" (name mode) "\")"))))
