(ns cml.core.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean difference]])
  (:import [cml.inference.hypothesis.critical_value Dependant Independant Welch RepeatedMeasure]
           [cml.statistics.variation Sample Pooled]
           [clojure.lang PersistentVector]))
(use 'criterium.core)

;TODO start documenting all functions

;TODO change function destructuring to samples etc are mapped accross

(defn dependant-ttest [{:keys [^PersistentVector data hypothetical-mean]}]
  (let [mean ^double (mean data)]
    (t-test (Dependant.
              mean
              (:standard-deviation (standard-deviation
                                     (Sample. mean data)))
              hypothetical-mean
              (count data)))))


(defn independant-ttest [{:keys [^PersistentVector samples ^PersistentVector hypothesized-population-means]}]
  (let [[sample-one sample-two] samples
        [hypothesized-population-mean-one hypothesized-population-mean-two] hypothesized-population-means
        [sample-mean-one sample-mean-two population-mean-one population-mean-two]
        (map mean [sample-one sample-two [hypothesized-population-mean-one] [hypothesized-population-mean-two]])
        [sample-size-one sample-size-two] (map count samples)]
    (t-test (Independant.
              [sample-mean-one sample-mean-two]
              [population-mean-one population-mean-two]
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


(defn repeated-measure-ttest [{:keys [^PersistentVector populations ^PersistentVector hypothesized-population-means]}]
  (let [[population-one population-two] populations
        [hypothesized-population-mean-one hypothesized-population-mean-two] hypothesized-population-means
        population-mean-difference ^double (mean (difference populations))
        population-means (mapv mean [[hypothesized-population-mean-one] [hypothesized-population-mean-two]])]
    (t-test
      (RepeatedMeasure.
        population-mean-difference
        population-means
        (:standard-deviation (standard-deviation
                               (Sample. population-mean-difference
                                        (difference populations))))
        (/ (+ (count population-one) (count population-two)) 2)))))


