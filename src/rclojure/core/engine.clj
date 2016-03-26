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
  "Takes a parameter
   for a single thread that connects
   to R. Starts a new thread if no
   threads are running"
  [thread-param]
  (let [re (Rengine/getMainEngine)]
    (if (nil? re)
      (try
        (new-thread thread-param)
        (Rengine/getMainEngine)) re)))


(defn eval-double-array [expr] (.asDoubleArray (.eval (engine "--vanilla") expr)))


(defn eval-matrix [expr] (.asMatrix (.eval (engine "--vanilla") expr)))


(defn eval-expr ([expr] (.eval (engine "--vanilla") expr)))


(defmacro assign [binding val] `(~'.assign ~'(engine "--vanilla") ~binding ~val))


