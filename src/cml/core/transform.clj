(ns cml.core.transform)


(defn xform-values [comp]
  (fn [map]
    (reduce-kv (fn [m k v]
                 (assoc m
                   k (comp v))) {} map)))


(defmacro xform-by-key [m fns]
  (cons (cons 'comp
              (for [i (range 0
                             (count fns))]
                `(~'fn [x#]
                   (~'update x#
                     ~@(fns i)))))
        `(~m)))


