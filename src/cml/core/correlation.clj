(ns cml.core.correlation
  (:require [cml.core.stats.utils :refer :all]))

(defn deviation-score [mean sample]
  (map (fn [x]  (- (mean sample) x)) sample))


(defn pearson-correlation [{:keys [x-axis y-axis]}]
  (/ (reduce + (map * x-axis y-axis))
     (Math/sqrt (* (reduce + (squared x-axis))
                   (reduce + (squared y-axis))))))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(def sample {:x-axis (deviation-score mean [490 500 530 550 580 590 600 600 650 700])
             :y-axis (deviation-score mean [560 500 510 600 600 620 550 630 650 750])})



