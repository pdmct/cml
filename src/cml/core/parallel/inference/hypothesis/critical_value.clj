(ns cml.core.parallel.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.inference.hypothesis.critical_value Dependant EqualVariance]
           [cml.statistics.variation Sample Pooled]))


(defn eq-var-t-test [[data-one data-two] [mean-one mean-two]]
  (pvalues
    (t-test (EqualVariance.
              [(mean data-one) (mean data-two)]             ;todo   ;try (pmap mean data) and try pmean with reducers
              [mean-one mean-two]
              [(:variance (variance (Pooled. (mean data-one) data-one (- (count data-one) 1))))
               (:variance (variance (Pooled. (mean data-two) data-two (- (count data-two) 1))))]
              [(count data-one) (count data-two)]))))


