(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))



(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn one-sample-t-test [hypothetical-mean sample]
  (/ (- (s/mean sample) (s/mean hypothetical-mean))
     (/ (s/standard-deviation {:type :sample} sample) (Math/sqrt (count sample)))))



;(defn one-sample-t-test :one-tail)
;(defn two-sample-t-test :one-tail)

;(defn one-sample-t-test :two-tail)
;(defn two-sample-t-test :two-tail)


