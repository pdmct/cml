(ns cml.core.sample
  (:require [bigml.sampling.simple :as simple]
            [bigml.sampling.reservoir :as reservoir]
            [bigml.sampling.stream :as stream]))



(defn random-dummy-sample
  ([population]
   (simple/sample population))
  ([population size]
    (stream/sample (range) size population)))


(defn random-population-sample [population size]
  (let [a (atom population)]
    (loop [x [] y size]
      (if (= y 0)
        x
        (do
          (swap! a pop)
          (recur (conj x (last (swap! a shuffle))) (dec y)))))))


