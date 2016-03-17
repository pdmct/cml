(ns rclojure.core
  (:require [rclojure.core.engine :refer [<- <<-]]
            [rclojure.core.rfn :as r]))


(defn rsum
  "Takes a sequence and returns the sum
   of a collection as a double or takes a
   sequence and a map where the map wiil
   explicitly define the return type. The
   return types are 32bit integeror a
   64 bit double"
  ([{:keys [coll type set]}]
   (<<- r/sum coll type set)))


(defn rabs
  "Function rabs(coll) computes the
   absolute value of the contents
   of coll"
  ([{:keys [coll type]}]
   (<- r/abs coll type)))


(defn rappend
  "Appendss coll1 on to coll. If set
   is applied, appends coll1 at the
   position of set"
  ([{:keys [coll coll1 type set]}]
   (<<- r/append coll coll1 type set)))


(defn rcat
  [{:keys [coll type set]}]
  (<<- r/cat coll type set))


