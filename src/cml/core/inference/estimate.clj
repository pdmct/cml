(ns cml.core.inference.estimate
  (:require [clojure.spec :as s]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.inference.estimate :refer [confidence-interval]]
            [cml.statistics.central-tendancy :refer [mean]])
  (:import [cml.statistics.variation Sample]
           [cml.inference.estimate OneSample TwoSample]))

(s/def ::sample (s/and coll? (s/coll-of number?)))
(s/def ::critical-value number?)
(s/def ::one-sample-conf-inter (s/keys :req [::sample ::critical-value]))


(defn data [sample critical-value]
  (if (s/valid? ::one-sample-conf-inter
                {::sample sample ::critical-value critical-value})
    {:sample sample :critical-value critical-value}
    (assert (s/explain ::one-sample-conf-inter
                       {::sample sample ::critical-value critical-value}))))

(defmulti data :source)

(defmethod data :one-sample-conf-inter [sample critical-value]
  ;Have these for every function as a means to validate data as an option dont bake in
  ;The reason behind seperating the data validion and functions is because
  ; when working with data there is two steps transformation and applying the algo
  ; we want feedback on whats wrong with the data not the function.
  )

(defn one-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (OneSample. (mean sample)
                                   (:standard-deviation (standard-deviation (Sample. (mean sample) sample)))
                                   (count sample) critical-value)))


(defn two-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (TwoSample. (map mean sample)
                                   (map #(:variance (variance (Sample. (mean %) %))) sample)
                                   (map count sample) critical-value)))


