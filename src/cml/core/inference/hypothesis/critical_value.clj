(ns cml.core.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.inference.hypothesis.critical_value Dependant EqualVariance]
           [cml.statistics.variation Sample Pooled]))

;TODO start documenting all functions

(defn dep-t-test [data hypothetical-mean]
  (let [mean ^double (mean data)]
    (t-test (Dependant.
              mean
              (:standard-deviation (standard-deviation (Sample. mean  data)))
              hypothetical-mean
              (count data)))))


(defn eq-var-t-test [{:keys [sample-one sample-two population-one population-two]}]
  (let [means (map mean [sample-one sample-two population-one population-two])
        counts (map count [sample-one sample-two])]
    (t-test (EqualVariance.
              [(first means) (second means)]
              [(nth means 2) (nth means 3)]
              [(:variance (variance (Pooled. (first means) sample-one (- (first counts) 1))))
               (:variance (variance (Pooled. (second means) sample-two (- (second counts) 1))))]
              [(first counts) (second counts)]))))



