(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.correlation :refer :all]
            [cml.core.utils.stats :refer :all]
            [cml.core.distribution.t :refer :all]
            [cml.core.hypothesis.test :refer :all]
            [cml.core.sample :refer :all]))


(def population-one [490 500 530 550 580 590 600 600 650 700])
(def sample-two [560 500 510 600 600 620 550 630 650 750])
(def ballet-dancers [89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3])
(def football-players [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9])

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


(deftest one-sample-t-test-test
  (is (= (one-sample-t-test {:mean      (mean population-one)
                             :sd        (:val (standard-deviation {:standard-deviation :sample} population-one))
                             :hypo-mean 400
                             :size      (count population-one)})
         {:mean 579.0, :hypo-mean 400, :sd 65.05553183413554, :size 10, :t-statistic 8.700992601418207})))


(deftest t-table-test
  (is (= (t-table {:dof 9 :alpha 0.05 :test :one-tail})
         {:dof 9, :alpha 0.05, :test :one-tail, :critical-val 1.8331})))


(deftest one-sample-conf-inter-test
  (is (= (one-sample-conf-inter {:mean         (mean population-one)
                                 :sd           (:val (standard-deviation {:standard-deviation :sample} population-one))
                                 :size         (count population-one)
                                 :critical-val 1.8331})
         {:mean 579.0, :sd 65.05553183413554, :size 10, :critical-val 1.8331, :plus 616.7112031961178, :minus 541.2887968038822})))

(deftest two-sample-t-test-equal-variance
  (is (= (two-sample-t-test {:two-sample-t-test :equal-variance}
                            {:s1-mean     (mean ballet-dancers)
                             :s1-pool-var (:val (variance {:variance :pooled} ballet-dancers))
                             :s1-size     (count ballet-dancers)}

                            {:s2-mean     (mean football-players)
                             :s2-pool-var (:val (variance {:variance :pooled} football-players))
                             :s2-size     (count football-players)})
         {:s1-mean     87.94999999999999,
          :s2-mean     85.19,
          :s1-pool-var 32.382777777777775,
          :s2-pool-var 31.181000000000015,
          :s1-size     10,
          :s2-size     10,
          :t-statistic 1.094722972460392,
          :exec        {:two-sample-t-test :equal-variance}})))


(deftest two-sample-t-test-unequal-variance
  (is (= (two-sample-t-test {:two-sample-t-test :unequal-variance}
                            {:s1-mean     (mean ballet-dancers)
                             :s1-pool-var (:val (variance {:variance :pooled} ballet-dancers))
                             :s1-size     (count ballet-dancers)}

                            {:s2-mean     (mean football-players)
                             :s2-pool-var (:val (variance {:variance :pooled} football-players))
                             :s2-size     (count football-players)})
         {:s1-mean     87.94999999999999,
          :s2-mean     85.19,
          :s1-pool-var 32.382777777777775,
          :s2-pool-var 31.181000000000015,
          :s1-size     10,
          :s2-size     10,
          :t-statistic 1.0947229724603922,
          :exec        {:two-sample-t-test :unequal-variance}})))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;WORKSPACE

;If t-statistic is greater than critical value we can reject the null hypothesis

(one-sample-t-test {:mean      (mean population-one)
                    :sd        (:val (standard-deviation {:standard-deviation :sample} population-one))
                    :hypo-mean 400
                    :size      (count population-one)})

(t-table {:dof 9 :alpha 0.05 :test :one-tail})


(one-sample-conf-inter {:mean         (mean population-one)
                        :sd           (:val (standard-deviation {:standard-deviation :sample} population-one))
                        :size         (count population-one)
                        :critical-val 1.8331})


(two-sample-t-test {:two-sample-t-test :equal-variance}
                   {:s1-mean     (mean ballet-dancers)
                    :s1-pool-var (:val (variance {:variance :pooled} ballet-dancers))
                    :s1-size     (count ballet-dancers)}

                   {:s2-mean     (mean football-players)
                    :s2-pool-var (:val (variance {:variance :pooled} football-players))
                    :s2-size     (count football-players)})


(two-sample-t-test {:two-sample-t-test :unequal-variance}
                   {:s1-mean     (mean ballet-dancers)
                    :s1-pool-var (:val (variance {:variance :pooled} ballet-dancers))
                    :s1-size     (count ballet-dancers)}

                   {:s2-mean     (mean football-players)
                    :s2-pool-var (:val (variance {:variance :pooled} football-players))
                    :s2-size     (count football-players)})


