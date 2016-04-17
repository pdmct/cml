(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn t-test [mean sd hypo-mean size alpha]
  {:mean      mean
   :hypo-mean hypo-mean
   :sd        sd
   :size      size
   :alpha     alpha})


(defn one-sample-t-test [{:keys [mean sd hypo-mean size alpha]}]
  ((comp (fn [x]
           (assoc x :test-stat
                    (/ (- (:mean x) (:hypo-mean x))
                       (/ (:sd x) (Math/sqrt (:size x)))))))
    (t-test mean
            sd
            hypo-mean
            size
            alpha)))


(defn conf-inter [mean sd size t]
  {:mean mean
   :sd   sd
   :size size
   :t    t})


(defn one-sample-conf-inter [{:keys [mean sd size t]}]
  ((comp (fn [x]
           (assoc x :plus (+ mean (* t (/ sd (Math/sqrt size))))
                    :minus (- mean (* t (/ sd (Math/sqrt size)))))))
    (conf-inter mean sd size t)))


