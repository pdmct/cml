(ns rclojure.core
  (:require [rclojure.core.engine :refer [reval rfn-coll->double-array rfn-coll-map->double-array rfn-coll2-map->double-array
                                                rfn-coll->int-array rfn-coll-map->int-array rfn-coll2-map->int-array
                                                rfn-coll2->double-array rfn-coll2->int-array]]
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
   (rfn-coll->double-array r/sum (double-array coll)))
  ([coll {:keys [int? double? rm-na?]}]
   (cond int?
         (rfn-coll-map->int-array r/sum (int-array coll) rm-na?)
         double?
         (rfn-coll-map->double-array r/sum (double-array coll) rm-na?))))


(defn rabs
  "Takes a sequence and computes
   the absolute value of x, sqrt(x)
   computes the (principal) square
   root of x, √{x}"
  ([coll]
   (rfn-coll->double-array r/abs (double-array coll)))
  ([coll {:keys [int? double?]}]
   (cond int?
         (rfn-coll->int-array r/abs (int-array coll))
         double?
         (rfn-coll->double-array r/abs (double-array coll)))))


(defn rappend
  "Takes a sequence and appends another
   sequence on to the end."
  ([coll1 coll2]
   (rfn-coll2->double-array r/append (double-array coll1) (double-array coll2)))
  ([coll1 coll2 {:keys [int? double?]}]
   (cond int?
         (rfn-coll2->int-array r/append (int-array coll1) (int-array coll2))
         double?
         (rfn-coll2->double-array r/append (double-array coll1) (double-array coll2)))))