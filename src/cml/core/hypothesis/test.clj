(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats :as s]))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn t-test [mean hypo-mean variance size alpha]
  {:mean       mean
   :hypo-mean hypo-mean
   :variance   variance
   :sample-size       size
   :dof (dec size)
   :alpha alpha})


(defn one-sample-t-test [{:keys [sample variance hypo-mean size alpha]}]
  ((comp (fn [x]
           (assoc x :t (/ (- (:mean x) (:hypo-mean x))
                          (/ (:variance x) (Math/sqrt (:sample-size x)))))))
    (t-test  sample
             hypo-mean
             variance
             size
             alpha)))


(defn conf-inter
  [mean variance size alpha cv]
  {:mean mean
   :variance variance
   :sample-size size
   :dof (dec size)
   :alpha alpha
   :critical-val cv})


(defn one-sample-conf-inter
  []
  (conf-inter 90 10 15 0.05 2.145))



{:plus (+ 90 (* 2.145 (/ 10 (Math/sqrt 15))))  :minus (- 90 (* 2.145 (/ 10 (Math/sqrt 15))))}


