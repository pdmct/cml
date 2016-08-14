(ns cml.core.numerical.estimate
  (:require [clojure.spec :as s]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.numerical.estimate :refer [confidence-interval]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.statistics.variation Sample]
           [cml.numerical.estimate OneSample TwoSample]))


(defn one-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (OneSample. (mean sample)
                                   (:standard-deviation (standard-deviation (Sample. (mean sample) sample)))
                                   (count sample) critical-value)))

(s/fdef one-sample-conf-inter
        :args {:sample         (s/and sequential? not-empty)
               :critical-value number?}
        :ret map?)


(defn two-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (TwoSample. (map mean sample)
                                   (map #(:variance (variance (Sample. (mean %) %))) sample)
                                   (map count sample) critical-value)))


(s/fdef two-sample-conf-inter
        :args {:sample         (or [sequential? sequential?] '(sequential? sequential?))
               :critical-value number?}
        :ret map?)


