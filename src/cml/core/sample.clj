(ns cml.core.sample
  (:require [bigml.sampling.simple :as simple]
            [bigml.sampling.reservoir :as reservoir]
            [bigml.sampling.stream :as stream]))

;TODO turn into multi method and call function sample

(defn rand-sample
  ([population]
   (simple/sample population))
  ([population size]
    (stream/sample (range) size population)))


