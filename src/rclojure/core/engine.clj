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


(defn- reval
  ([expr] (.eval (engine "--vanilla") expr))
  ([expr {:keys [type]}]
   (cond
     (= type :double-array)
     (.asDoubleArray (.eval (engine "--vanilla") expr))
     (= type :int-array)
     (.asIntArray (.eval (engine "--vanilla") expr)))))


(defmacro rassign
  [binding val]
  `(~'.assign ~'(engine "--vanilla") ~binding ~val))

;TODO have coll be incorporated into type map

(defn rfn-exec
  ([rfn coll type]
   (let [gs (str (gensym))]
     (try
       (cond (= (:type type) :double-array)
             (try
               (rassign gs (double-array coll))
               (reval (rfn gs) type)
               (finally (reval (r/remove gs))))
             (= (:type type) :int-array)
             (try
               (rassign gs (int-array coll))
               (reval (rfn gs) type)
               (finally (reval (r/remove gs)))))))))


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
