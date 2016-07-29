(ns cml.core.parallel.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.inference.hypothesis.critical_value Dependant EqualVariance]
           [cml.statistics.variation Sample Pooled]))


(defn dep-t-test [{:keys [data hypothetical-mean]}]
  (pvalues
    (t-test (Dependant.
              (mean data)
              (:standard-deviation (standard-deviation (Sample. (mean data) data)))
               hypothetical-mean
              (count data)))))


(defn eq-var-t-test [{:keys [sample-one sample-two population-one population-two]}]
  (pvalues
    (t-test (EqualVariance.
              [(mean sample-one) (mean sample-two)]             ;todo   ;try (pmap mean data) and try pmean with reducers
              [(mean population-one) (mean population-two)]
              [(:variance (variance (Pooled. (mean sample-one) sample-one (- (count sample-one) 1))))
               (:variance (variance (Pooled. (mean sample-two) sample-two (- (count sample-two) 1))))]
              [(count sample-one) (count sample-two)]))))


