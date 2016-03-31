(ns rclojure.core
  (:require [rclojure.core.engine
             :refer [eval-double-array eval-int-array eval-matrix eval-expr contents]]
            [rclojure.core.coll-expression :refer :all]
            [rclojure.core.fn-expression :as fn]))


(defn rsum
  ([{:keys [coll type set]}]
   (if (nil? set)
     (if (= type :double)
       (seq (contents (fn/sum (as-vec coll {:mode :double}))))
       (seq (contents (fn/sum (as-vec coll {:mode :integer})))))
     (if (= type :double)
       (seq (contents (fn/sum (as-vec coll {:mode :double}) set)))
       (seq (contents (fn/sum (as-vec coll {:mode :integer}) set)))))))


(defn rabs ([coll] (seq (contents (fn/abs coll)))))


(defn rappend
  ([{:keys [coll coll1 type set]}]
   (if (nil? set)
     (if (= type :double)
       (seq
         (contents
           (fn/append
             (as-vec coll {:mode :double})
             (as-vec coll1 {:mode :double}))))
       (seq
         (contents
           (fn/append
             (as-vec coll {:mode :integer})
             (as-vec coll1 {:mode :integer}) set)))))))

   (defn rcat
     ([{:keys [coll set]}]
      (if (nil? set)
        (println coll)
        (seq (contents (fn/cat (as-vec coll) set))))))


(defn rmatrix [{:keys [coll set]}] (map seq (eval-matrix (fn/matrix coll set))))


(defn rplot-vec
  [{:keys [coll file set]}]
  (if (= file :jpg)
        (do
          (eval-expr (fn/jpg set))
          (eval-expr (fn/plot-vec (as-vec coll)))
          (eval-expr (fn/dev-off)))))


(defn rplot-matrix
  [{:keys [coll file type set]}]
  (if (= file :jpg)
    (do
      (eval-expr (fn/jpg set))
      (eval-expr (fn/plot (fn/matrix (as-vec coll) set)))
      (eval-expr (fn/dev-off)))))


