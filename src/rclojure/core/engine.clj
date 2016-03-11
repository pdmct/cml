(ns rclojure.core.engine
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.rfn :as r]))

(defn- new-thread
  [thread-param]
  (Rengine. (into-array [thread-param]) false nil))

(defn- engine
  [thread-param]
  (let [re (Rengine/getMainEngine)]
    (if (nil? re)
      (try
        (new-thread thread-param)
        (Rengine/getMainEngine)) re)))

(defn- reval [expr] (.eval (engine "--vanilla") expr))

(defmacro rassign
  [binding val]
  `(~'.assign ~'(engine "--vanilla") ~binding ~val))


(defn rfn-exec
  ([rfn coll]
   (cond (contains? coll :coll1)
         (let [gs (str (gensym))]
           (try
             (rassign gs (double-array (:coll1 coll)))
             (.asDoubleArray (reval (rfn gs)))
             (finally (reval (r/remove gs)))))
         (every? coll [:coll1 :coll2])
         (let [gs1 (str (gensym)) gs2 (str (gensym))]
           (try
             (rassign gs1 (double-array (:coll1 coll)))
             (rassign gs1 (double-array (:coll2 coll)))
             (.asDoubleArray (reval (rfn gs1 gs1)))
             (finally (reval (r/remove gs1 gs2))))))))


#_(defn rfn-coll-map->double-array
  ([rfn coll set]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asDoubleArray (reval (rfn gs set)))
       (finally (reval (r/remove gs)))))))


#_(defn rfn-coll2-map->double-array
  ([rfn coll1 coll2 set]
   (let [gs1 (str (gensym)) gs2 (str (gensym))]
     (try
       (rassign gs1 coll1)
       (rassign gs2 coll2)
       (.asDoubleArray (reval (rfn gs1 gs2 set)))
       (finally (reval (r/remove gs1 gs2)))))))


#_(defn rfn-coll->int-array
  ([rfn coll]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asIntArray (reval (rfn gs)))
       (finally (reval (r/remove gs)))))))


#_(defn rfn-coll-map->int-array
  ([rfn coll set]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asIntArray (reval (rfn gs set)))
       (finally (reval (r/remove gs)))))))


#_(defn rfn-coll2-map->int-array
  ([rfn coll1 coll2 set]
   (let [gs1 (str (gensym)) gs2 (str (gensym))]
     (try
       (rassign gs1 coll1)
       (rassign gs2 coll2)
       (.asIntArray (reval (rfn gs1 gs2 set)))
       (finally (reval (r/remove gs1 gs2)))))))

#_(defn rfn-coll2->double-array
  ([rfn coll1 coll2]
   (let [gs1 (str (gensym)) gs2 (str (gensym))]
     (try
       (rassign gs1 coll1)
       (rassign gs2 coll2)
       (.asDoubleArray (reval (rfn gs1 gs2)))
       (finally (reval (r/remove gs1 gs2)))))))


#_(defn rfn-coll2->int-array
  ([rfn coll1 coll2]
   (let [gs1 (str (gensym)) gs2 (str (gensym))]
     (try
       (rassign gs1 coll1)
       (rassign gs2 coll2)
       (.asIntArray (reval (rfn gs1 gs2)))
       (finally (reval (r/remove gs1 gs2)))))))
