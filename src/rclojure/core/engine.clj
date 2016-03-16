(ns rclojure.core.engine
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.rfn :as r]))


(defn- new-thread
  "Starts a single thread that manages
   a connection to R"
  [thread-param]
  (Rengine. (into-array [thread-param]) false nil))


(defn- engine
  "Function that takes a parameter
   for a single thread that connects
   to R. Starts a new thread if no
   threads are running"
  [thread-param]
  (let [re (Rengine/getMainEngine)]
    (if (nil? re)
      (try
        (new-thread thread-param)
        (Rengine/getMainEngine)) re)))


(defn- reval
  "Function that takes an parameterised
   R function as a parameter. The R function
   is evaluated by the R interperator. The
   return type of double array or int array
   can be set explicitly or defaults to
   double array"
  ([expr] (.eval (engine "--vanilla") expr))
  ([expr type]
   (cond
     (= type :double-array)
     (.asDoubleArray (.eval (engine "--vanilla") expr))
     (= type :int-array)
     (.asIntArray (.eval (engine "--vanilla") expr)))))


(defmacro rassign
  "Function that assigns String binding
   to a double array or an int array"
  [binding val]
  `(~'.assign ~'(engine "--vanilla") ~binding ~val))


(defn <-
  "Function that executes rfn over the
   collection coll or collections coll
   and coll1. A return type can be
   explicitly defined or defaults
   to double array"
  ([rfn coll type]
   (let [gs (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (reval (rfn gs) type)
               (finally (reval (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (reval (rfn gs) type)
               (finally (reval (r/remove gs))))))))
  ([rfn coll coll1 type]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (rassign gs1 (double-array coll1))
               (reval (rfn gs gs1) type)
               (finally (reval (r/remove gs gs1))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (rassign gs1 (int-array coll1))
               (reval (rfn gs gs1) type)
               (finally (reval (r/remove gs gs1)))))))))


(defn <<-
  "Function that executes rfn over the
   collection coll or collections coll
   and coll1. Applys an extra arg to
   rfn allowing extra settings to be
   applied. A return type can be
   explicitly defined or defaults
   to double array"
  ([rfn coll type set]
   (let [gs (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (reval (rfn gs set) type)
               (finally (reval (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (reval (rfn gs set) type)
               (finally (reval (r/remove gs))))))))
  ([rfn coll coll1 type set]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (rassign gs1 (double-array coll1))
               (reval (rfn gs gs1 set) type)
               (finally (reval (r/remove gs gs1))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (rassign gs1 (int-array coll1))
               (reval (rfn gs gs1 set) type)
               (finally (reval (r/remove gs gs1)))))))))


