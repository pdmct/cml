(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))

;TODO Equal variances not assumed for two-sample-t-test

(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn one-sample [mean sd hypo-mean size alpha]
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
    (one-sample mean
                sd
                hypo-mean
                size
                alpha)))


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



(defn two-sample [s1-mean s2-mean s1-pooled-variance
                  s2-poooled-variance s1-size s2-size]
  {:s1-mean            s1-mean
   :s2-mean            s2-mean
   :s1-pooled-variance s1-pooled-variance
   :s2-pooled-variance s2-poooled-variance
   :s1-size            s1-size
   :s2-size            s2-size})


(defn two-sample-t-test [{:keys [s1-mean s1-pooled-variance s1-size]}
                         {:keys [s2-mean s2-pooled-variance s2-size]}]

  ((comp (fn [x]
           (assoc x :t (Math/abs (/ (- (:s1-mean x) (:s2-mean x))
                                    (Math/sqrt (* (/ (+ (:s1-pooled-variance x) (:s2-pooled-variance x)) 2)
                                                  (+ (/ 1 (:s1-size x))
                                                     (/ 1 (:s2-size x))))))))))
    (two-sample s1-mean s2-mean s1-pooled-variance
                s2-pooled-variance s1-size s2-size)))

