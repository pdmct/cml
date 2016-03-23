(ns rclojure.core
  (:require [rclojure.core.engine
             :refer [as-double-array as-int-array as-int-array-set
                     as-double-array-set as-double-array->file
                     as-int-array->file as-matrix]]
            [rclojure.core.rfn :as r]))


(defn rsum
  "Returns the sum of a collection"
  ([{:keys [coll type set]}]
   (cond (= type :double-array)
         (as-double-array-set r/sum coll set)
         (= type :int-array)
         (as-int-array-set r/sum coll set))))


(defn rabs
  "Function computes the absolute
   value of the contents of coll"
  ([{:keys [coll type]}]
   (cond (= type :double-array)
         (as-double-array r/abs coll)
         (= type :int-array)
         (as-int-array r/abs coll))))


(defn rappend
  "Appendss coll1 on to coll. If set
   is applied, appends coll1 at the
   position of set"
  ([{:keys [coll coll1 type set]}]
   (cond (= type :double-array)
         (as-double-array-set r/append coll coll1 set)
         (= type :int-array)
         (as-int-array-set r/append coll coll1 set))))


(defn rcat
  "Outputs the objects, concatenating
   the representations"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (as-double-array-set r/cat coll set)
        (= type :int-array)
        (as-int-array-set r/cat coll set)))


(defn rmatrix
  [{:keys [coll set]}]
  (as-matrix r/matrix coll set))


(defn rplot-jpg
  "Plots a collection to a jpeg file"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (as-double-array->file r/plot r/jpg coll set)
        (= type :int-array)
        (as-int-array->file r/plot r/jpg coll set)))


(defn rplot-pdf
  "Plots a collection to a pdf file"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (as-double-array->file r/plot r/pdf coll set)
        (= type :int-array)
        (as-int-array->file r/plot r/pdf coll set)))


