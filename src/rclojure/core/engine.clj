(ns rclojure.core.engine
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.rfn :as r]))

;TODO allow user to set gs in rfn-exec-graph as this is displayed in the plot x axis
;TODO start looking at more complex data type eg multi dim arrays and data frames

;(evaluate "matrix(one, nrow = 2, ncol = 3, byrow = TRUE)")
;(.asMatrix (evaluate "matrix(one, nrow = 2, ncol = 3, byrow = TRUE)"))

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


(defprotocol rcoll-type
  (double-vec [c] "R vector of double")
  (int-vec [c] "R vector of double int")
  (matrix [c] "R matrix"))


(defrecord evaluate-rcoll [expr]
  rcoll-type
  (double-vec [this] (.asDoubleArray (.eval (engine "--vanilla") expr)))
  (int-vec [this] (.asIntArray (.eval (engine "--vanilla") expr)))
  (matrix [this] (.asMatrix (.eval (engine "--vanilla") expr))))


(defn evaluate
  "Function that takes an parameterised
   R function as a parameter."
  ([expr] (.eval (engine "--vanilla") expr)))

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
               (double-vec (->evaluate-rcoll (rfn gs)))
               (finally (evaluate (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (int-vec (->evaluate-rcoll (rfn gs)))
               (finally (evaluate (r/remove gs))))))))
  ([rfn coll coll1 type]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (rassign gs1 (double-array coll1))
               (double-vec (->evaluate-rcoll (rfn gs gs1)))
               (finally (evaluate (r/remove gs gs1))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (rassign gs1 (int-array coll1))
               (int-vec (->evaluate-rcoll (rfn gs gs1)))
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
               (double-vec (->evaluate-rcoll (rfn gs set)))
               (finally (evaluate (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (int-vec (->evaluate-rcoll (rfn gs set)))
               (finally (evaluate (r/remove gs))))))))
  ([rfn coll coll1 type set]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (cond (= type :double-array)
             (try
               (rassign gs (double-array coll))
               (rassign gs1 (double-array coll1))
               (double-vec (->evaluate-rcoll (rfn gs gs1 set)))
               (finally (evaluate (r/remove gs gs1))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (rassign gs1 (int-array coll1))
               (int-vec (->evaluate-rcoll (rfn gs gs1 set)))
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
               (double-vec (->evaluate-rcoll (rfn gs)))
               (evaluate "dev.off()")
               (finally (evaluate (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (evaluate (rfn2 set))
               (int-vec (->evaluate-rcoll (rfn gs)))
               (evaluate "dev.off()")
               (finally (evaluate (r/remove gs)))))))))


