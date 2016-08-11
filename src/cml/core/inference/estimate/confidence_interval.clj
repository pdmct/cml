(ns cml.core.inference.estimate.confidence-interval
  (:require [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.inference.estimate.confidence-interval :refer [confidence-interval]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.statistics.variation Sample]
           [cml.inference.estimate.confidence_interval OneSample TwoSample]))


(defn one-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (OneSample. (mean sample)
                                   (:standard-deviation (standard-deviation (Sample. (mean sample) sample)))
                                   (count sample) critical-value)))


(defn two-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (TwoSample. (map mean sample)
                                   (map #(:variance (variance (Sample. (mean %) %))) sample)
                                   (map count sample) critical-value)))

