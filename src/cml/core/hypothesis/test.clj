(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn t-test [sample hm]
  {:sample-mean (s/mean sample)
   :hypothetical-mean (s/mean hm)
   :sample-variance (s/variance {:type :sample} sample)
   :sample-size (count sample)})


(defn one-sample-t-test [sample mean]
  ((comp (fn [x] (assoc x :t
                          (/ (- (:sample-mean x) (:hypothetical-mean x))
                             (/ (:sample-variance x) (Math/sqrt (:sample-size x)))))))
    (t-test sample mean)))


;(defn one-sample-t-test :one-tail)
;(defn two-sample-t-test :one-tail)
;(defn one-sample-t-test :two-tail)
;(defn two-sample-t-test :two-tail)


