(ns cml.core.inference.hypothesis.critical-value
  (:require [cml.inference.hypothesis.critical-value :refer [t-test]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.inference.hypothesis.critical_value Dependant EqualVariance]
           [cml.statistics.variation Sample Pooled]))


(defn dep-t-test [data hypothetical-mean]
  (let [mean (mean data)]
    (t-test (Dependant.
              mean
              (:standard-deviation (standard-deviation (Sample. mean  data)))
              hypothetical-mean
              (count data)))))


(defn eq-var-t-test2 [[data-one data-two] [mean-one mean-two]]
  (let [mean-data-one (mean data-one)
        mean-data-two (mean data-two)
        count-data-one (count data-one)
        count-data-two (count data-two)]                    ;TODO refactor
    (t-test (EqualVariance.
              [mean-data-one mean-data-two]
              [mean-one mean-two]
              [(:variance (variance (Pooled. mean-data-one data-one (- count-data-one 1))))
               (:variance (variance (Pooled. mean-data-two data-two  (- count-data-two 1))))]
              [count-data-one count-data-two]))))

