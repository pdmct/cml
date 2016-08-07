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
              (:standard-deviation (standard-deviation (Sample. mean data)))
              h-mean
              (count data)))))

;Make Multi Method this function is equal variance ttest
(defn two-sample-ttest [{:keys [^PersistentVector sample ^PersistentVector hp-mean]}]
  (t-test (TwoSample.
            (map mean sample)
            (map mean (partition 1 hp-mean))
            (map #(:variance (variance (Pooled. (mean %) % (- (count %) 1)))) sample)
            (map count sample))))



(defn welch-ttest [{:keys [^PersistentVector sample]}]
  (t-test (Welch. (map mean sample)
                  (map #(:variance (variance (Sample. (mean %) %))) sample)
                  (map count sample))))


(defn rep-measure-ttest [{:keys [^PersistentVector population ^PersistentVector hp-mean]}]
  (let [[population-one population-two] population
        population-mean-difference ^double (mean (difference population))]
    (t-test (RepeatedMeasure.
              population-mean-difference
              (map mean (partition 1 hp-mean))
              (:standard-deviation (standard-deviation (Sample. population-mean-difference (difference population))))
        (/ (+ (count population-one) (count population-two)) 2)))))


