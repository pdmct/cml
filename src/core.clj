(ns rclojure.core
  (:require [rclojure.core.engine
             :refer [eval-double-array eval-matrix]]
            [rclojure.core.rfn :as r]))


(defn rsum
  ([{:keys [coll type set]}]
   (if (nil? set)
     (cond (= type :double-array)
           (eval-double-array (r/sum coll))
           (= type :int-array)
           (int-array (eval-double-array (r/sum coll))))
     (cond (= type :double-array)
           (eval-double-array (r/sum coll set))
           (= type :int-array)
           (int-array (eval-double-array (r/sum coll set)))))))


(defn rabs
  ([{:keys [coll type]}]
   (cond (= type :double-array)
         (eval-double-array (r/abs coll))
         (= type :int-array)
         (int-array (eval-double-array (r/abs coll))))))


(defn rappend
  ([{:keys [coll coll1 type set]}]
   (if (nil? set)
     (cond (= type :double-array)
           (eval-double-array (r/append coll coll1))
           (= type :int-array)
           (int-array (eval-double-array (r/append coll coll1))))
     (cond (= type :double-array)
           (eval-double-array (r/append coll coll1 set))
           (= type :int-array)
           (int-array (eval-double-array (r/append coll coll1 set)))))))


(defn rcat
  ([{:keys [coll type set]}]
   (if (nil? set)
     (cond (= type :double-array)
           (eval-double-array (r/cat coll))
           (= type :int-array)
           (int-array (eval-double-array (r/cat coll))))
     (cond (= type :double-array)
           (eval-double-array (r/cat coll set))
           (= type :int-array)
           (int-array (eval-double-array (r/cat coll set)))))))

(defn rmatrix [{:keys [coll set]}] (eval-matrix (r/matrix coll set)))


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


