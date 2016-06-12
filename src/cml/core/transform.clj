(ns cml.core.transform)


(defmacro xform-all [m & fns]
  `(reduce-kv (fn [m1# k# v#]
                (assoc m1# k# ((comp ~@fns) v#))) {} ~m))

(defmacro xform-by-key-test [m & kfns]
  (cons (cons 'comp
              (for [i (range 0 (count kfns))]
                `(~'fn [x#] (~'update x# :department clojure.string/upper-case))))
        `(~m)))


