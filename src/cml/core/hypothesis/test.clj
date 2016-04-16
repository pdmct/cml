(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn t-test [mean hm variance ss alpha]
  {:mean       mean
   :hypothetical-mean hm
   :sample-variance   variance
   :sample-size       ss
   :dof (dec ss)
   :alpha alpha})


(defn one-sample-t-test [{:keys [sample hypothetical-mean alpha]}]
  ((comp (fn [x]
           (assoc x :t (/ (- (:mean x) (:hypothetical-mean x))
                          (/ (:sample-variance x) (Math/sqrt (:sample-size x)))))))
    (t-test (s/mean sample)
            (s/mean hypothetical-mean)
            (s/variance {:type :sample} sample)
            (count sample)
            alpha)))


;(defn one-sample-t-test :one-tail)
;(defn two-sample-t-test :one-tail)
;(defn one-sample-t-test :two-tail)
;(defn two-sample-t-test :two-tail)

