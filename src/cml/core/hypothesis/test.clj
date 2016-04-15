(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))



(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn one-sample-t-test [est-population-mean sample]
  (/ (- (s/mean sample) est-population-mean)
     (/ (s/standard-deviation {:type :sample} sample) (Math/sqrt (count sample)))))


