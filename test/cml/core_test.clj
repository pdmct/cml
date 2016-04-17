(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.correlation :refer :all]
            [cml.core.utils.stats :refer :all]
            [cml.core.distribution.t :refer :all]
            [cml.core.hypothesis.test :refer :all]
            [cml.core.sample :refer :all]))

(def population-one [490 500 530 550 580 590 600 600 650 700])
(def sample-two [560 500 510 600 600 620 550 630 650 750])

(def sample {:x-axis (deviation-score mean [490 500 530 550 580 590 600 600 650 700])
             :y-axis (deviation-score mean [560 500 510 600 600 620 550 630 650 750])})

(pearson-correlation sample)

(significance (pearson-correlation sample) 8)

(t-table {:dof 8 :alpha 0.05 :test :two-tail})

(coefficient-determination (pearson-correlation sample))

(def a (random-population-sample population-one 4))

(one-sample-t-test {:mean (mean population-one)
                    :variance (variance {:type :sample} population-one)
                    :hypo-mean 400
                    :size (count population-one)
                    :alpha 0.05})
(count a)

(t-table {:dof 9 :alpha 0.05 :test :one-tail})

(variance {:type :sample} [490,500,530,550,580,590,600,600,650,700])

(conf-inter 90 10 15 0.05 2.145)


