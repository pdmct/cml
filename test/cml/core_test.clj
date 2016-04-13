(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.correlation :refer :all]
            [cml.core.utils.stats :refer :all]
            [cml.core.distribution.t :refer :all]
            [cml.core.hypothesis.test :refer :all]
            [cml.core.sample :refer :all]))

(def sample-one [490 500 530 550 580 590 600 600 650 700])
(def sample-two [560 500 510 600 600 620 550 630 650 750])

(def sample {:x-axis (deviation-score mean [490 500 530 550 580 590 600 600 650 700])
             :y-axis (deviation-score mean [560 500 510 600 600 620 550 630 650 750])})

(pearson-correlation sample)

(significance (pearson-correlation sample) 8)

(t-table 8 0.05 :two-tail)

(coefficient-determination (pearson-correlation sample))


(t-distribution sample-one [490 500 530 550])


