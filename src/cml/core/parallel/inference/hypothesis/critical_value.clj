(ns cml.core.parallel.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean difference]]
            [clojure.core.reducers :as r])
  (:import [cml.inference.hypothesis.critical_value OneSample TwoSample Welch RepeatedMeasure]
           [cml.statistics.variation Sample Pooled]
           [clojure.lang PersistentVector]))

;TODO ammend params as non parallel versions

(defn one-sample-ttest [{:keys [^PersistentVector data hypo-mean]}]
  (pvalues
    (t-test (OneSample.
              (mean data)
              (:standard-deviation (standard-deviation (Sample. (mean data) data)))
              hypo-mean
              (count data)))))


(defn two-sample-ttest [{:keys [^PersistentVector samples ^PersistentVector populations]}]
  (pvalues
    (t-test (TwoSample.
              (map mean samples)
              (map mean (partition 1 populations))
              (map #(:variance (variance (Pooled. (mean %) % (- (count %) 1)))) samples)
              (map count samples)))))


(defn welch-ttest [{:keys [^PersistentVector samples]}]
  (pvalues
    (t-test (Welch. (map mean samples)
                    (map #(:variance (variance (Sample. (mean %) %))) samples)
                    (map count samples)))))


(defn repeated-measure-ttest [{:keys [^PersistentVector populations ^PersistentVector hp-means]}]
  (pvalues
    (t-test (RepeatedMeasure. (mean (difference populations))
                              (map mean (partition 1 hp-means))
                              (:standard-deviation (standard-deviation (Sample. (mean (difference populations)) (difference populations))))
                              (/ (r/fold + (r/map count populations)) 2)))))


