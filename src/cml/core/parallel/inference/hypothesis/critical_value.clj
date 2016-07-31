(ns cml.core.parallel.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.inference.hypothesis.critical_value Dependant Independant Welch]
           [cml.statistics.variation Sample Pooled]
           [clojure.lang PersistentVector]))

;TODO ammend params as non parallel versions

(defn dep-t-test [{:keys [^PersistentVector data hypothetical-mean]}]
  (pvalues
    (t-test (Dependant.
              (mean data)
              (:standard-deviation (standard-deviation (Sample. (mean data) data)))
               hypothetical-mean
              (count data)))))


(defn eq-var-t-test [{:keys [^PersistentVector sample-one ^PersistentVector sample-two ^PersistentVector population-one ^PersistentVector population-two]}]
  (pvalues
    (t-test (Independant.
              [(mean sample-one) (mean sample-two)]
              [(mean population-one) (mean population-two)]
              [(:variance (variance (Pooled. (mean sample-one) sample-one (- (count sample-one) 1))))
               (:variance (variance (Pooled. (mean sample-two) sample-two (- (count sample-two) 1))))]
              [(count sample-one) (count sample-two)]))))


(defn welch-t-test [{:keys [^PersistentVector sample-one ^PersistentVector sample-two]}]
  (pvalues
    (t-test (Welch. [(mean sample-one) (mean sample-two)]
                    [(:variance (variance (Sample. (mean sample-one) sample-one)))
                     (:variance (variance (Sample. (mean sample-two) sample-two)))]
                    [(count sample-two) (count sample-two)]))))


