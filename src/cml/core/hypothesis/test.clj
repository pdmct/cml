(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn t-test [mean variance hypo-mean size alpha]
  {:mean      mean
   :hypo-mean hypo-mean
   :variance  variance
   :size      size
   :alpha     alpha})


(defn one-sample-t-test [{:keys [mean variance hypo-mean size alpha]}]
  ((comp (fn [x]
           (assoc x :t (/ (- (:mean x) (:hypo-mean x))
                          (/ (:variance x) (Math/sqrt (:size x)))))))
    (t-test  mean
             variance
             hypo-mean
             size
             alpha)))


(defn conf-inter [mean variance size t]
  {:mean mean
   :variance variance
   :size size
   :t t})


(defn one-sample-conf-inter [{:keys [mean variance size t]}]
  ((comp (fn [x]
           (assoc x :plus (+ mean (* t (/ variance (Math/sqrt size))))
                    :minus  (- mean (* t (/ variance (Math/sqrt size)))))))
    (conf-inter mean variance size t)))


