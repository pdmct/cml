(ns cml.core.transform)


(defn transform-values [f]
  (fn [m]
    (reduce-kv (fn [map key val]
                 (assoc map
                   key
                   (f val))) {} m)))


(defn transform-by-key
  ([k f]
   (fn [m]
     (assoc m k (f (k m)))))
  ([k f x]
   (fn [m]
     (assoc m k (f (k m) x))))
  ([k f x y]
   (fn [m]
     (assoc m k (f (k m) x x y))))
  ([k f x y z]
   (fn [m]
     (assoc m k (f (k m) x x y z))))
  ([k f x y z & more]
   (fn [m]
     (assoc m k (apply f (k m) x x y z more)))))


