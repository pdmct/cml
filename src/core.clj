(ns rclojure.core
  (:require [rclojure.core.engine :refer [rfn-exec rfn-exec+]]
            [rclojure.core.cols :refer [rvec]]
            [rclojure.core.rfn :as r]))

;TODO cat wont print to console so use clojures print or insist file output

(defn rsum
  "Takes a sequence and returns the sum
   of a collection as a double or takes a
   sequence and a map where the map wiil
   explicitly define the return type. The
   return types are 32bit integeror a
   64 bit double"
  ([coll]
   (rfn-exec r/sum coll :double-array))
  ([coll type]
   (cond (= type :int-array)
         (rfn-exec r/sum coll type)
         (= type :double-array)
         (rfn-exec r/sum coll type)))
  ([coll type set]
   (cond (= type :int-array)
         (rfn-exec+ r/sum coll type set)
         (= type :double-array)
         (rfn-exec+ r/sum coll type set))))


(defn rabs
  "rabs(coll) computes the absolute value
   of the contents of coll"
  ([coll]
   (rfn-exec r/abs coll :double-array))
  ([coll type]
   (cond (= type :int-array)
         (rfn-exec r/abs coll type)
         (= type :double-array)
         (rfn-exec r/abs coll type))))