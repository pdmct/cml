(ns rclojure.core.engine
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.rfn :as r]))

;TODO allow user to set gs in rfn-exec-graph as this is displayed in the plot x axis
;TODO start looking at more complex data type eg multi dim arrays and data frames

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


(defn evaluate
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


(defmacro ^{:private true} rassign
  "Function that assigns String binding
   to a double array or an int array"
  [binding val]
  `(~'.assign ~'(engine "--vanilla") ~binding ~val))


(defn rfn-exec
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
               (evaluate (rfn gs) type)
               (finally (evaluate (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (evaluate (rfn gs) type)
               (finally (evaluate (r/remove gs))))))))
  ([rfn coll coll1 type]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (rassign gs1 (double-array coll1))
               (evaluate (rfn gs gs1) type)
               (finally (evaluate (r/remove gs gs1))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (rassign gs1 (int-array coll1))
               (evaluate (rfn gs gs1) type)
               (finally (evaluate (r/remove gs gs1)))))))))


(defn rfn-exec-set
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
               (evaluate (rfn gs set) type)
               (finally (evaluate (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (evaluate (rfn gs set) type)
               (finally (evaluate (r/remove gs))))))))
  ([rfn coll coll1 type set]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (rassign gs1 (double-array coll1))
               (evaluate (rfn gs gs1 set) type)
               (finally (evaluate (r/remove gs gs1))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (rassign gs1 (int-array coll1))
               (evaluate (rfn gs gs1 set) type)
               (finally (evaluate (r/remove gs gs1)))))))))


(defn rfn-exec-graph
  "Function that executes rfn over the
   collection coll or collections coll
   and coll1. A return type can be
   explicitly defined or defaults
   to double array"
  ([rfn rfn2 coll type set]
   (let [gs (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (evaluate (rfn2 set))
               (evaluate (rfn gs) type)
               (evaluate "dev.off()")
               (finally (evaluate (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (evaluate (rfn2 set))
               (evaluate (rfn gs) type)
               (evaluate "dev.off()")
               (finally (evaluate (r/remove gs)))))))))


