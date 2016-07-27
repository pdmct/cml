(ns cml.core.parallel.central-tendancy
  (:require [clojure.core.reducers :as r]))

(defn pmean [data] (pvalues (double (/ (r/fold + data) (count data)))))

(defn pmean-1 [data] (pvalues (double (/ (r/fold + data) (dec (count data))))))

(defn pdifference [[sample-one sample-two]] (pmap - sample-one sample-two))


