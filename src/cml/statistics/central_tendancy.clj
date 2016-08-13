(ns cml.statistics.central-tendancy
  (:use [criterium.core])
  (:require [cml.utils :refer [double-asum]]
            [clojure.core.reducers :as r]))
(use 'criterium.core)

(defn ^double mean [data] (/ (r/fold + data) (count data)))

(defn ^double mean-1 [data] (/ (r/fold + data) (dec (count data))))

(defn ^doubles difference [[sample-one sample-two]] (map - sample-one sample-two))

(defn permutations
  [x xs]                                                    ;TODO Put in respectable NS
  (letfn [(factorial [x]
            (loop [cnt (if (coll? x)
                         (count x) x) acc 1]
              (if (zero? cnt)
                acc (recur (dec cnt) (*' cnt acc)))))]
    (quot (factorial x)
          (factorial (- x xs)))))


(defn significance [correlation sample-size]
  (/ (* correlation
        (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


