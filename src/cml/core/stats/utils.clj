(ns cml.core.stats.utils)

(defn squared
  ([coll] (map #(* % %) coll))
  ([f coll] (map #(* % %) (f coll))))

(defn sum
  ([coll] (reduce + coll))
  ([f coll] (reduce + (f coll))))

(defn mean [sample] (quot (reduce + sample) (count sample)))

