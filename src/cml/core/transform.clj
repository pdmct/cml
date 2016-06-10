(ns cml.core.transform)


(defmacro xform-all [m & args]
  `(reduce-kv (fn [m1# k# v#]
                (assoc m1# k# ((comp ~@args) v#))) {} ~m))


