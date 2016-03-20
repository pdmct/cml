(ns rclojure.core
  (:require [rclojure.core.engine :refer [rfn->double-array rfn->int-array rfn-set->int-array
                                          rfn-set->double-array rfn-exec-graph evaluate]]
            [rclojure.core.rfn :as r]))


(defn rsum
  "Takes a sequence and returns the sum
   of a collection as a double or takes a
   sequence and a map where the map wiil
   explicitly define the return type. The
   return types are 32bit integeror a
   64 bit double"
  ([{:keys [coll type set]}]
   (cond (= type :double-array)
         (rfn-set->double-array r/sum coll set)
         (= type :int-array)
         (rfn-set->int-array r/sum coll set))))


(defn rabs
  "Function rabs(coll) computes the
   absolute value of the contents
   of coll"
  ([{:keys [coll type]}]
   (cond (= type :double-array)
         (rfn->double-array r/abs coll)
         (= type :int-array)
         (rfn->int-array r/abs coll))))


(defn rappend
  "Appendss coll1 on to coll. If set
   is applied, appends coll1 at the
   position of set"
  ([{:keys [coll coll1 type set]}]
   (cond (= type :double-array)
         (rfn-set->double-array r/append coll coll1 set)
         (= type :int-array)
         (rfn-set->int-array r/append coll coll1 set))))


(defn rcat
  "Outputs the objects, concatenating
   the representations"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (rfn-set->double-array r/cat coll set)
        (= type :int-array)
        (rfn-set->int-array r/cat coll set)))


(defn rplot->jpg
  [{:keys [coll type set]}]
  (rfn-exec-graph r/plot r/jpg coll type set))


(defn rplot->pdf
  [{:keys [coll type set]}]
  (rfn-exec-graph r/plot r/pdf coll type set))


