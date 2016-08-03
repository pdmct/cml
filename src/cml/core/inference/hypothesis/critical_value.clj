(ns cml.core.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test significance]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean difference]])
  (:import [cml.inference.hypothesis.critical_value OneSample TwoSample Welch RepeatedMeasure OneTail TwoTail]
           [cml.statistics.variation Sample Pooled]
           [clojure.lang PersistentVector]))
(use 'criterium.core)

;TODO start documenting all functions

(defn one-sample-ttest [{:keys [^PersistentVector data h-mean]}]
  (let [mean ^double (mean data)]
    (t-test (OneSample.
              mean
              (:standard-deviation (standard-deviation
                                     (Sample. mean data)))
              h-mean
              (count data)))))


(defn two-sample-ttest [{:keys [^PersistentVector samples ^PersistentVector hp-means]}]
  (let [[sample-one sample-two] samples
        [sample-mean-one sample-mean-two] (map mean samples)
        [sample-size-one sample-size-two] (map count samples)]
    (t-test (TwoSample.
              [sample-mean-one sample-mean-two]
              (map mean (partition 1 hp-means))
              [(:variance (variance
                            (Pooled. sample-mean-one sample-one
                                     (- sample-size-one 1))))
               (:variance (variance
                            (Pooled. sample-mean-two sample-two
                                     (- sample-size-two 1))))]
              [sample-size-one sample-size-two]))))



(defn welch-ttest [{:keys [^PersistentVector samples]}]
  (let [[sample-one sample-two] samples
        [sample-mean-one sample-mean-two] (map mean samples)
        [sample-count-one sample-count-two] (map count samples)]
    (t-test (Welch. [sample-mean-one sample-mean-two]
                    [(:variance (variance
                                  (Sample. sample-mean-one sample-one)))
                     (:variance (variance
                                  (Sample. sample-mean-two sample-two)))]
                    [sample-count-one sample-count-two]))))


(defn rep-measure-ttest [{:keys [^PersistentVector populations ^PersistentVector hp-means]}]
  (let [[population-one population-two] populations
        population-mean-difference ^double (mean (difference populations))]
    (t-test
      (RepeatedMeasure.
        population-mean-difference
        (map mean (partition 1 hp-means))
        (:standard-deviation (standard-deviation
                               (Sample. population-mean-difference
                                        (difference populations))))
        (/ (+ (count population-one) (count population-two)) 2)))))


(defn one-tail-sig-test [{:keys [dof alpha]}] (significance (OneTail. dof alpha)))


(defn two-tail-sig-test [{:keys [dof alpha]}] (significance (TwoTail. dof alpha)))


