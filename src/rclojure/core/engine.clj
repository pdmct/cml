(ns rclojure.core.engine
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.rfn :as r]))

(def new-thread
  (Rengine. (into-array ["--vanilla"]) false nil))

(defn reval
  [expr]
  (let [re (Rengine/getMainEngine)]
    (try
      (.eval re expr)
      (finally (.end re)))))

(defmacro rassign
  [binding val]
  `(~'.assign ~'(Rengine/getMainEngine) ~binding ~val))

;TODO make it so dynamically set .asDoubleArray or .asIntArray to remove duplicate code
;TODO rname functions accordingly

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
