(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.correlation :refer :all]
            [cml.core.utils.stats :refer :all]
            [cml.core.distribution.t :refer :all]
            [cml.core.hypothesis.test :refer :all]
            [cml.core.utils.samples :refer :all]
            [cml.core.sample :refer :all]))

(def sample {:x-axis (deviation-score mean [490 500 530 550 580 590 600 600 650 700])
             :y-axis (deviation-score mean [560 500 510 600 600 620 550 630 650 750])})

(def rand-ballet (random-population-sample ballet-dancers 4))
(def rand-football (random-population-sample football-players 4))


(deftest pearson-correlation-test
  (is (= (pearson-correlation sample) 0.8702620996632292)))


(deftest sig-pearson-correlation-test
  (is (= (significance (pearson-correlation sample) 8))))


(deftest t-table-test
  (is (= (:critical-val (t-table {:dof 8 :alpha 0.05 :test :two-tail})) 2.306)))


(deftest coefficient-determination-pearson-test
  (is (= (coefficient-determination (pearson-correlation sample)) 0.7573561221102523)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(one-sample-t-test {:mean      (mean population-one)
                    :sd        (standard-deviation {:type :sample} population-one)
                    :hypo-mean 400
                    :size      (count population-one)
                    :alpha     0.05})                       ; => critical-val = 1.8331

(t-table {:dof 9 :alpha 0.05 :test :one-tail})

(one-sample-conf-inter {:mean (mean population-one)
                        :sd   (standard-deviation {:type :sample} population-one)
                        :size (count population-one)
                        :critical-val   1.8331})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

