(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn t-test [mean hm variance ss]
  {:sample-mean       mean
   :hypothetical-mean hm
   :sample-variance   variance
   :sample-size       ss})


(defn one-sample-t-test [sample mean]
  ((comp (fn [x]
           (assoc x :t (/ (- (:sample-mean x) (:hypothetical-mean x))
                          (/ (:sample-variance x) (Math/sqrt (:sample-size x)))))))
    (t-test (s/mean sample)
            (s/mean mean)
            (s/variance {:type :sample} sample)
            (count sample))))


;(defn one-sample-t-test :one-tail)
;(defn two-sample-t-test :one-tail)
;(defn one-sample-t-test :two-tail)
;(defn two-sample-t-test :two-tail)

