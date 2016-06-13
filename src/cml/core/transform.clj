(ns cml.core.transform)


(defmacro xform-values [m fns]
  `(reduce-kv (fn [m1# k# v#]
                (assoc m1#
                  k# ((comp ~@fns)
                       v#))) {} ~m))


(defmacro xform-by-key [m fns]                             ;TODO look at (conj s v v1) instead of (cons (cons ..) ..)
  (cons (cons 'comp
              (for [i (range 0
                             (count fns))]
                `(~'fn [x#]
                   (~'update x#
                     ~@(fns i)))))
        `(~m)))


