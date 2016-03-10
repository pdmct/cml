(ns rclojure.core.engine
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.rfn :as r]))


(defn- engine []
  (if (nil? (Rengine/getMainEngine))
    (try
      (Rengine. (into-array ["--vanilla"]) false nil)
       (Rengine/getMainEngine))
     (Rengine/getMainEngine)))

(defn- reval [expr] (.eval (engine) expr))

(defmacro rassign
  [binding val]
  `(~'.assign ~'(engine) ~binding ~val))

;TODO make it so dynamically set .asDoubleArray or .asIntArray to remove duplicate code can then try overloding again
;TODO rname functions accordingly (rfn-assaign-exec  and rfn-multi-assign-exec)

(defn rfn-coll->double-array
  ([rfn coll]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asDoubleArray (reval (rfn gs)))
       (finally (reval (r/remove gs)))))))


(defn rfn-coll-map->double-array
  ([rfn coll set]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asDoubleArray (reval (rfn gs set)))
       (finally (reval (r/remove gs)))))))


(defn rfn-coll2-map->double-array
  ([rfn coll1 coll2 set]
   (let [gs1 (str (gensym)) gs2 (str (gensym))]
     (try
       (rassign gs1 coll1)
       (rassign gs2 coll2)
       (.asDoubleArray (reval (rfn gs1 gs2 set)))
       (finally (reval (r/remove gs1 gs2)))))))


(defn rfn-coll->int-array
  ([rfn coll]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asIntArray (reval (rfn gs)))
       (finally (reval (r/remove gs)))))))


(defn rfn-coll-map->int-array
  ([rfn coll set]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asIntArray (reval (rfn gs set)))
       (finally (reval (r/remove gs)))))))


(defn rfn-coll2-map->int-array
  ([rfn coll1 coll2 set]
   (let [gs1 (str (gensym)) gs2 (str (gensym))]
     (try
       (rassign gs1 coll1)
       (rassign gs2 coll2)
       (.asIntArray (reval (rfn gs1 gs2 set)))
       (finally (reval (r/remove gs1 gs2)))))))

(defn rfn-coll2->double-array
  ([rfn coll1 coll2]
   (let [gs1 (str (gensym)) gs2 (str (gensym))]
     (try
       (rassign gs1 coll1)
       (rassign gs2 coll2)
       (.asDoubleArray (reval (rfn gs1 gs2)))
       (finally (reval (r/remove gs1 gs2)))))))


(defn rfn-coll2->int-array
  ([rfn coll1 coll2]
   (let [gs1 (str (gensym)) gs2 (str (gensym))]
     (try
       (rassign gs1 coll1)
       (rassign gs2 coll2)
       (.asIntArray (reval (rfn gs1 gs2)))
       (finally (reval (r/remove gs1 gs2)))))))
