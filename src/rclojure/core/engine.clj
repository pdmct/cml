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


(defrecord evaluate-coll [expr]
  rcoll-type
  (double-vec [this] (.asDoubleArray (.eval (engine "--vanilla") expr)))
  (int-vec [this] (.asIntArray (.eval (engine "--vanilla") expr)))
  (matrix [this] (.asMatrix (.eval (engine "--vanilla") expr))))


(defn evaluate-expr
  "Function that takes an parameterised
   R function as a parameter."
  ([expr] (.eval (engine "--vanilla") expr)))

(defmacro ^{:private true} rassign
  "Function that assigns String binding
   to a double array or an int array"
  [binding val]
  `(~'.assign ~'(engine "--vanilla") ~binding ~val))

(defn rfn->double-array
  ([rfn coll]
   (let [gs (str (gensym))]
     (try
       (rassign gs (double-array coll))
       (double-vec (->evaluate-coll (rfn gs)))
       (finally (evaluate-expr (r/remove gs))))))
  ([rfn coll coll1]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (rassign gs (double-array coll))
       (rassign gs1 (double-array coll1))
       (double-vec (->evaluate-coll (rfn gs gs1)))
       (finally (evaluate-expr (r/remove gs gs1)))))))


(defn rfn->int-array
  ([rfn coll]
   (let [gs (str (gensym))]
     (try
       (rassign gs (int-array coll))
       (int-vec (->evaluate-coll (rfn gs)))
       (finally (evaluate-expr (r/remove gs))))))
  ([rfn coll coll1]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (rassign gs (int-array coll))
       (rassign gs1 (int-array coll1))
       (int-vec (->evaluate-coll (rfn gs gs1)))
       (finally (evaluate-expr (r/remove gs gs1)))))))


(defn rfn-set->double-array
  ([rfn coll set]
   (let [gs (str (gensym))]
     (try
       (rassign gs (double-array coll))
       (double-vec (->evaluate-coll (rfn gs set)))
       (finally (evaluate-expr (r/remove gs))))))
  ([rfn coll coll1 set]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (rassign gs (double-array coll))
       (rassign gs1 (double-array coll1))
       (double-vec (->evaluate-coll (rfn gs gs1 set)))
       (finally (evaluate-expr (r/remove gs gs1)))))))


(defn rfn-set->int-array
  ([rfn coll set]
   (let [gs (str (gensym))]
     (try
       (rassign gs (int-array coll))
       (int-vec (->evaluate-coll (rfn gs set)))
       (finally (evaluate-expr (r/remove gs))))))
  ([rfn coll coll1 set]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (rassign gs (int-array coll))
       (rassign gs1 (int-array coll1))
       (int-vec (->evaluate-coll (rfn gs gs1 set)))
       (finally (evaluate-expr (r/remove gs gs1)))))))


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
               (evaluate-expr (rfn2 set))
               (double-vec (->evaluate-coll (rfn gs)))
               (evaluate-expr "dev.off()")
               (finally (evaluate-expr (r/remove gs))))
             (= type :int-array)
             (try
               (rassign gs (int-array coll))
               (evaluate-expr (rfn2 set))
               (int-vec (->evaluate-coll (rfn gs)))
               (evaluate-expr "dev.off()")
               (finally (evaluate-expr (r/remove gs)))))))))


