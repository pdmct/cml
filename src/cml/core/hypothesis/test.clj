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
           (assoc x :t-value
                    (/ (- (:mean x) (:hypo-mean x))
                       (/ (:sd x) (Math/sqrt (:size x)))))))
    (t-test mean
            sd
            hypo-mean
            size
            alpha)))


(defn two-sample-t-test [{:keys [s1-p-mean s1-s-mean]} {:keys [s2-p-mean s2-s-mean]}]
  (- (- (mean {:type :sample} s1-s-mean)
        (mean {:type :sample} s2-s-mean))
     (- (mean {:type :population} s1-p-mean)
        (mean {:type :population} s2-p-mean))))



(defn conf-inter [mean sd size critical-val]
  {:mean mean
   :sd   sd
   :size size
   :critical-val    critical-val})


(defn one-sample-conf-inter [{:keys [mean sd size critical-val]}]
  ((comp (fn [x]
           (assoc x :plus (+ mean (* critical-val (/ sd (Math/sqrt size))))
                    :minus (- mean (* critical-val (/ sd (Math/sqrt size)))))))
    (conf-inter mean sd size critical-val)))


