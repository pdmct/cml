(ns cml.core.parallel.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean difference]]
            [clojure.core.reducers :as r])
  (:import [cml.inference.hypothesis.critical_value Dependant Independant Welch RepeatedMeasure]
           [cml.statistics.variation Sample Pooled]
           [clojure.lang PersistentVector]))

;TODO ammend params as non parallel versions

(defn dep-t-test [{:keys [^PersistentVector data hypothetical-mean]}]
  (pvalues
    (t-test (Dependant.                                     ;TODO change to OneSample
              (mean data)
              (:standard-deviation (standard-deviation (Sample. (mean data) data)))
               hypothetical-mean
              (count data)))))


(defn independant-t-test [{:keys [^PersistentVector samples ^PersistentVector populations]}]
  (pvalues
    (t-test (Independant.                                   ;TODO change to TwoSample
              (map mean samples)
              (map mean (partition 1 populations))
              (map #(:variance (variance (Pooled. (mean %) % (- (count %) 1)))) samples)
              (map count samples)))))


(defn welch-t-test [{:keys [^PersistentVector samples]}]
  (pvalues
    (t-test (Welch. (map mean samples)
                    (map #(:variance (variance (Sample. (mean %) %))) samples)
                    (map count samples)))))


(defn repeated-measure-ttest [{:keys [^PersistentVector populations ^PersistentVector hypothesized-population-means]}]
  (pvalues
    (t-test (RepeatedMeasure. (mean (difference populations))
                              (map mean (partition 1 hypothesized-population-means))
                              (:standard-deviation (standard-deviation
                                                     (Sample. (mean (difference populations))
                                                              (difference populations))))
                              (/ (r/fold + (r/map count populations)) 2)))))


