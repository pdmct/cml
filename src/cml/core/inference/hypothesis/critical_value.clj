(ns cml.core.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.inference.hypothesis.critical_value Dependant EqualVariance Welch]
           [cml.statistics.variation Sample Pooled]
           [clojure.lang PersistentVector]))

;TODO start documenting all functions

(defn dep-t-test [{:keys [^PersistentVector data hypothetical-mean]}]
  (let [mean ^double (mean data)]
    (t-test (Dependant.
              mean
              (:standard-deviation (standard-deviation
                                     (Sample. mean data)))
              hypothetical-mean
              (count data)))))


(defn eq-var-t-test [{:keys [^PersistentVector sample-one ^PersistentVector sample-two ^PersistentVector population-one ^PersistentVector population-two]}]
  (let [[sample-mean-one sample-mean-two population-mean-one population-mean-two] (map mean [sample-one sample-two population-one population-two])
        [sample-size-one sample-size-two] (map count [sample-one sample-two])]
    (t-test (EqualVariance.
              [sample-mean-one sample-mean-two]
              [population-mean-one population-mean-two]
              [(:variance (variance
                            (Pooled. sample-mean-one sample-one
                                     (- sample-size-one 1))))
               (:variance (variance
                            (Pooled. sample-mean-two sample-two
                                     (- sample-size-two 1))))]
              [sample-size-one sample-size-two]))))



(defn welch-t-test [{:keys [^PersistentVector sample-one ^PersistentVector sample-two]}]
  (let [[sample-mean-one sample-mean-two] (map mean [sample-one sample-two])
        [sample-count-one sample-count-two] (map count [sample-one sample-two])]
    (t-test (Welch. [sample-mean-one sample-mean-two]
                    [(:variance (variance
                                  (Sample. sample-mean-one sample-one)))
                     (:variance (variance
                                  (Sample. sample-mean-two sample-two)))]
                    [sample-count-one sample-count-two]))))


