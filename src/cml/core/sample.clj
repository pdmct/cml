(ns cml.core.sample
  (:require [bigml.sampling.simple :as simple]
            [bigml.sampling.reservoir :as reservoir]
            [bigml.sampling.stream :as stream]))


(defn random-dummy-sample
  ([population]
   (simple/sample population))
  ([population size]
    (stream/sample (range) size population)))


(defn random-population-sample [population size]            ;TODO use java api
  (let [x (atom []) p (atom population) s (atom size)]
    (while (not= @s 0)
      (do
        (swap! x conj (last (swap! p shuffle)))
        (swap! p pop)
        (swap! s dec))) @x))


