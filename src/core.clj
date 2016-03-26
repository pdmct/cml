(ns rclojure.core
  (:require [rclojure.core.engine
             :refer [evaluate-coll]]
            [rclojure.core.rfn :as r]))


(defn rsum
  "Returns the sum of a collection"
  ([{:keys [coll type set]}]
   (if (nil? set)
     (cond (= type :double-array)
           (evaluate-coll (r/sum coll))
           (= type :int-array)
           (int-array (evaluate-coll (r/sum coll))))
     (cond (= type :double-array)
           (evaluate-coll (r/sum coll set))
           (= type :int-array)
           (int-array (evaluate-coll (r/sum coll set)))))))


(defn rabs
  "Returns the sum of a collection"
  ([{:keys [coll type]}]
   (cond (= type :double-array)
         (evaluate-coll (r/abs coll))
         (= type :int-array)
         (int-array (evaluate-coll (r/abs coll))))))



#_(defn rabs
  "Function computes the absolute
   value of the contents of coll"
  ([{:keys [coll type]}]
   (cond (= type :double-array)
         (as-double-array r/abs coll)
         (= type :int-array)
         (as-int-array r/abs coll))))


#_(defn rappend
  "Appendss coll1 on to coll. If set
   is applied, appends coll1 at the
   position of set"
  ([{:keys [coll coll1 type set]}]
   (cond (= type :double-array)
         (as-double-array-set r/append coll coll1 set)
         (= type :int-array)
         (as-int-array-set r/append coll coll1 set))))


#_(defn rcat
  "Outputs the objects, concatenating
   the representations"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (as-double-array-set r/cat coll set)
        (= type :int-array)
        (as-int-array-set r/cat coll set)))


#_(defn rmatrix
  [{:keys [coll set]}]
  (as-matrix r/matrix coll set))


#_(defn rplot-jpg
  "Plots a collection to a jpeg file"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (double-array->file r/plot r/jpg coll set)
        (= type :int-array)
        (int-array->file r/plot r/jpg coll set)
        (= type :matrix)
        (matrix->file r/plot r/jpg "matrix(c(1,2,3,4))" set)))

;cant plot matrices?
#_(defn rplot-pdf
  "Plots a collection to a pdf file"
  [{:keys [coll type set]}]
  (cond (= type :double-array)
        (double-array->file r/plot r/pdf coll set)
        (= type :int-array)
        (int-array->file r/plot r/pdf coll set)))


