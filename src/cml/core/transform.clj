(ns cml.core.transform)


(defmacro xform-values [m fns]
  `(reduce-kv (fn [m1# k# v#]
                (assoc m1# k# ((comp ~@fns) v#))) {} ~m))


(defmacro xform-by-key [m kfns]
  (cons (cons 'comp
              (for [i (range 0 (count kfns))]
                `(~'fn [x#] (~'update x# ~@(kfns i)))))
        `(~m)))


