(ns rclojure.core
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.engine :refer [reval]]
            [rclojure.core.cols :refer [rvec]]
            [rclojure.core.rfn :as r]))

;TODO cat wont print to console so use clojures print or insist file output
;TODO change rassign to take the try catch finally and put code block in try

(defmacro rlet
  "Binds a symbol to an evaluated
   R function."
  [binding body]
  `(~'do
     (~'.assign ~'(Rengine/getMainEngine) ~(str (first binding)) ~(second binding))
     ~body))


(defmacro rassign
  [binding val]
  `(~'.assign ~'(Rengine/getMainEngine) ~binding ~val))

(defn rsum
  "Takes a sequence and returns the sum
   of a collection as a double or takes a
   sequence and a map where the map wiil
   explicitly define the return type. The
   return types are 32bit integeror a
   64 bit double."
  ([col]
   (let [gs (str (gensym))]
     (try
       (rassign gs (double-array col))
       (.asDoubleArray (reval (r/sum gs)))
       (finally (reval (r/remove gs))))))
  ([col {:keys [integer? double? rm-na?]}]
   (let [gs (str (gensym))]
     (cond (= integer? true)
           (try
             (rassign gs (int-array col))
             (.asIntArray (reval (r/sum gs rm-na?)))
             (finally (reval (r/remove gs))))
           (= double? true)
           (try
             (rassign gs (double-array col))
             (.asDoubleArray (reval (r/sum gs rm-na?)))
             (finally (reval (r/remove gs))))))))


(defn rabs
  "Takes a sequence and computes
   the absolute value of x, sqrt(x)
   computes the (principal) square
   root of x, √{x}"
  ([col]
   (let [gs (str (gensym))]
     (try
       (rassign gs (double-array col))
       (.asDoubleArray (reval (r/abs gs)))
       (finally (reval (r/remove gs))))))
  ([col {:keys [integer? double?]}]
    (let [gs (str (gensym))]
      (cond (= integer? true)
            (try
              (rassign gs (int-array col))
              (.asIntArray (reval (r/abs gs)))
              (finally (reval (r/remove gs))))
            (= double? true)
            (try
              (rassign gs (double-array col))
              (.asDoubleArray (reval (r/abs gs)))
              (finally (reval (r/remove gs))))))))


(defn rappend
  "Takes a sequence and appends another
   sequence on to the end."
  ([col col1]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (try
       (rassign gs (double-array col))
       (rassign gs1 (double-array col1))
       (.asDoubleArray (reval (r/append gs gs1)))
       (finally (reval (r/remove gs gs1))))))
  ([col col1 {:keys [integer? double?]}]
   (let [gs (str (gensym)) gs1 (str (gensym))]
     (cond (= integer? true)
           (try
             (rassign gs (int-array col))
             (rassign gs1 (int-array col1))
             (.asIntArray (reval (r/append gs gs1)))
             (finally (reval (r/remove gs gs1))))
           (= double? true)
           (try
             (rassign gs (double-array col))
             (rassign gs1 (double-array col1))
             (.asDoubleArray (reval (r/append gs gs1)))
             (finally (reval (r/remove gs gs1))))))))
