(ns rclojure.core
  (:import [org.rosuda.JRI Rengine])
  (:require [rclojure.core.engine :refer [reval]]
            [rclojure.core.cols :refer [rvec]]
            [rclojure.core.rfn :as r]))

;TODO cat wont print to console so use clojures print or insist file output

(defmacro rassign
  [binding val]
  `(~'.assign ~'(Rengine/getMainEngine) ~binding ~val))

(defn- rfn-exec->double-array
  ([rfn coll]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asDoubleArray (reval (rfn gs)))
       (finally (reval (r/remove gs))))))
  ([rfn coll set]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asDoubleArray (reval (rfn gs set)))
       (finally (reval (r/remove gs)))))))


(defn- rfn-exec->int-array
  ([rfn coll]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asIntArray (reval (rfn gs)))
       (finally (reval (r/remove gs))))))
  ([rfn coll set]
   (let [gs (str (gensym))]
     (try
       (rassign gs coll)
       (.asIntArray (reval (rfn gs set)))
       (finally (reval (r/remove gs)))))))


(defn rsum
  "Takes a sequence and returns the sum
   of a collection as a double or takes a
   sequence and a map where the map wiil
   explicitly define the return type. The
   return types are 32bit integeror a
   64 bit double."
  ([col]
   (rfn-exec->double-array r/sum (double-array col)))
  ([col {:keys [integer? double? rm-na?]}]
   (cond integer?
         (rfn-exec->int-array r/sum (int-array col) rm-na?)
         double?
         (rfn-exec->double-array r/sum (double-array col) rm-na?))))


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
