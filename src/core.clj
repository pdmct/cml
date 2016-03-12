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
   64 bit double."
  ([coll]
   (rfn-exec r/sum coll {:type :double-array}))
  ([coll {:keys [type]}]
   (cond (= type :int)
         (rfn-exec r/sum coll {:type :int-array})
         (= type :double)
         (rfn-exec r/sum coll {:type :double-array})))
  ([coll {:keys [type]} set]
   (cond (= type :int)
         (rfn-exec+ r/sum coll {:type :int-array} set)
         (= type :double)
         (rfn-exec+ r/sum coll {:type :double-array} set))))


