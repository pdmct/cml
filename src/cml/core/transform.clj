(ns cml.core.transform)


(defmacro xform-all [m & fns]
  `(reduce-kv (fn [m1# k# v#]
                (assoc m1# k# ((comp ~@fns) v#))) {} ~m))


(defn xform-by-key [])
