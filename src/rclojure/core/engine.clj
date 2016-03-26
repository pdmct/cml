(ns rclojure.core.engine
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.rfn :as r]))

;TODO allow user to set gs in rfn-exec-graph as this is displayed in the plot x axis
;TODO start looking at data frames


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


(defn evaluate-coll
  [expr]
  (.asDoubleArray (.eval (engine "--vanilla") expr)))


(defn evaluate-expr
  "Function that takes an parameterised
   R function as a parameter."
  ([expr] (.eval (engine "--vanilla") expr)))


(defmacro  rassign
  "Function that assigns String binding
   to a double array or an int array"
  [binding val]
  `(~'.assign ~'(engine "--vanilla") ~binding ~val))


#_(evaluate-expr "jpeg(\"/Users/gra11/foo.jpg\")")
#_(evaluate-expr "plot(matrix(c(1,2,3,11,12,13), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c(\"foo\",\"bar\"),c(\"mss\",\"poo\",\"wae\"))))")
#_(evaluate-expr "dev.off()")

#_(defn matrix->file
  ([coll]
   (let [gs (str (gensym))]
     (try
       (evaluate-expr "jpeg(\"/Users/gra11/foo.jpg\")")
       (evaluate-expr (str "plot(matrix(" coll ", nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c(\"foo\",\"bar\"),c(\"mss\",\"poo\",\"wae\"))))"))
       (evaluate-expr "dev.off()")))))

