(ns cml.core.data.specifications
  (:require [clojure.spec :as s]))


(s/def ::sample (s/and sequential? not-empty))
(s/def ::critical-value number?)
(s/def ::one-sample-conf-inter (s/keys :req [::sample ::critical-value]))


(defmulti data :Source)

(defmethod data :OneSampleConfInter [{:keys [sample critical-value]}]
  (if (s/valid? ::one-sample-conf-inter
                {::sample sample ::critical-value critical-value})
    {:sample sample :critical-value critical-value}
    (assert (s/explain-data ::one-sample-conf-inter
                            {::sample sample ::critical-value critical-value}))))

