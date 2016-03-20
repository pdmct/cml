(ns rclojure.core
  (:require [rclojure.core.engine :refer [rfn->double-array rfn->int-array rfn-set->int-array
                                          rfn-set->double-array rfn-set-double-array->file
                                          rfn-set-int-array->file]]
            [rclojure.core.rfn :as r]))


(defn rsum
  "Returns the sum of a collection"
  ([{:keys [coll type set]}]
   (cond (= type :double-array)
         (rfn-set->double-array r/sum coll set)
         (= type :int-array)
         (rfn-set->int-array r/sum coll set))))


(defn rabs
  "Function computes the absolute
   value of the contents of coll"
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
  "Plots a collection to a jpeg file"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (rfn-set-double-array->file r/plot r/jpg coll set)
        (= type :int-array)
        (rfn-set-int-array->file r/plot r/jpg coll set)))


(defn rplot->pdf
  "Plots a collection to a pdf file"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (rfn-set-double-array->file r/plot r/pdf coll set)
        (= type :int-array)
        (rfn-set-int-array->file r/plot r/pdf coll set)))


