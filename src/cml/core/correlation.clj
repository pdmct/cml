(ns cml.core.correlation
  (:require [cml.core.utils.stats :refer :all]
            [cml.core.utils.math :refer :all]
            [cml.core.accuracy :refer :all]))



(defn deviation-score [mean sample]
  (map (fn [x] (- (mean sample) x)) sample))


(defn pearson-correlation [{:keys [x-axis y-axis]}]
  (/ (reduce + (map * x-axis y-axis))
     (Math/sqrt (* (reduce + (squared x-axis))
                   (reduce + (squared y-axis))))))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


