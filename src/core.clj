(ns rclojure.core
  (:require [rclojure.core.engine
             :refer [eval-double-array eval-int-array eval-matrix eval-expr]]
            [rclojure.core.rfn :as r]))


(defn rsum
  ([{:keys [coll type set]}]
   (if (nil? set)
     (cond (= type :double-array)
           (seq (eval-double-array (r/sum coll)))
           (= type :int-array)
           (seq (eval-int-array (r/sum coll))))
     (cond (= type :double-array)
           (seq (eval-double-array (r/sum coll set)))
           (= type :int-array)
           (seq (eval-int-array (r/sum coll set)))))))


(defn rabs
  ([{:keys [coll type]}]
   (cond (= type :double-array)
         (seq (eval-double-array (r/abs coll)))
         (= type :int-array)
         (seq (eval-int-array (r/abs coll))))))


(defn rappend
  ([{:keys [coll coll1 type set]}]
   (if (nil? set)
     (cond (= type :double-array)
           (seq (eval-double-array (r/append coll coll1)))
           (= type :int-array)
           (seq (eval-int-array (r/append coll coll1))))
     (cond (= type :double-array)
           (seq (eval-double-array (r/append coll coll1 set)))
           (= type :int-array)
           (seq (eval-int-array (r/append coll coll1 set)))))))



(defn rcat
  ([{:keys [coll type set]}]
   (if (nil? set)
     (cond (= type :double-array)
           (println coll)
           (= type :int-array)
           (println coll))
     (cond (= type :double-array)
           (seq (eval-double-array (r/cat coll set)))
           (= type :int-array)
           (seq (eval-int-array (r/cat coll set)))))))


(defn rmatrix [{:keys [coll set]}] (map seq (eval-matrix (r/matrix coll set))))


(defn rplot-vec
  [{:keys [coll file set]}]
  (cond (= file :jpg)
        (do
          (eval-expr (r/jpg set))
          (eval-expr (r/plot-vec coll))
          (eval-expr (r/dev-off)))))


(defn rplot-matrix
  [{:keys [coll file set]}]
  (cond (= file :jpg)
        (do
          (eval-expr (r/jpg set))
          (eval-expr (r/plot (r/matrix coll set)))
          (eval-expr (r/dev-off)))))


