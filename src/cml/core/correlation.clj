(ns cml.core.correlation
  (:require [cml.core.stats.utils :refer :all]))

(def sample {:x [490 500 530 550 580 590 600 600 650 700]
             :y [560 500 510 600 600 620 550 630 650 750 ]})

(defn- deviation-score [mean sample]
  (map (fn [x]  (- (mean sample) x)) sample))


(defn pearson-correlation [{:keys [x y]}]
  (/ (reduce + (map * (deviation-score mean x) (deviation-score mean y)))
     (Math/sqrt (* (reduce + (squared (deviation-score mean x)))
                   (reduce + (squared (deviation-score mean y)))))))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))