(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.correlation :refer :all]
            [cml.core.utils.stats :refer :all]
            [cml.core.distribution.t :refer :all]
            [cml.core.inference.estimation.confidence :refer :all]
            [cml.core.inference.hypothesis.critical-value :refer :all]
            [cml.core.sample :refer :all]))


(def population-one [490 500 530 550 580 590 600 600 650 700])
(def sample-two [560 500 510 600 600 620 550 630 650 750])

(def ballet-dancers [89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3])
(def football-players [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9])

(def pre-college [104 106 105 100 110 100 110 108 103 101])
(def post-college [113 105 105 114 109 113 109 108 113 106])

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
  (is (= (one-sample-t-test {:mean               (mean population-one)
                             :standard-deviation (:val (standard-deviation {:standard-deviation :sample} population-one))
                             :hypo-mean          400
                             :size               (count population-one)})

         {:mean               579.0,
          :standard-deviation 65.05553183413554,
          :hypo-mean          400,
          :size               10,
          :dof                9,
          :type               :one-sample,
          :t-statistic        8.700992601418207})))


(deftest t-table-test
  (is (= (t-table {:dof 9 :alpha 0.05 :test :one-tail})
         {:dof 9, :alpha 0.05, :test :one-tail, :critical-val 1.8331})))


(deftest two-sample-t-test-equal-variance
  (is (= (two-sample-t-test {:two-sample-t-test :equal-variance}
                            {:mean            [(mean ballet-dancers) (mean football-players)]
                             :pooled-variance [(:val (variance {:variance :pooled} ballet-dancers)) (:val (variance {:variance :pooled} football-players))]
                             :size            [(count ballet-dancers) (count football-players)]})

         {:mean            [87.94999999999999 85.19],
          :pooled-variance [32.382777777777775 31.181000000000015],
          :size            [10 10],
          :dof             18,
          :type            :two-sample,
          :t-statistic     1.094722972460392,
          :exec            {:two-sample-t-test :equal-variance}}))) ;TODO test in SPSS


(deftest two-sample-t-test-unequal-variance
  (is (= (two-sample-t-test {:two-sample-t-test :unequal-variance}
                            {:mean            [(mean ballet-dancers) (mean football-players)]
                             :pooled-variance [(:val (variance {:variance :pooled} ballet-dancers)) (:val (variance {:variance :pooled} football-players))]
                             :size            [(count ballet-dancers) (count football-players)]})

         {:mean            [87.94999999999999 85.19],
          :pooled-variance [32.382777777777775 31.181000000000015],
          :size            [10 10],
          :dof             18,
          :type            :two-sample,
          :t-statistic     1.0947229724603922,
          :exec            {:two-sample-t-test :unequal-variance}}))) ;TODO test in SPSS


(deftest two-sample-confidence-interval-test
  (is (= (two-sample-confidence-interval {:mean         [(mean ballet-dancers) (mean football-players)]
                                          :variance     [(:val (variance {:variance :pooled} ballet-dancers)) (:val (variance {:variance :pooled} football-players))]
                                          :size         [(count ballet-dancers) (count football-players)]
                                          :critical-val 2.1009})

         {:mean         [87.94999999999999 85.19],
          :variance     [32.382777777777775 31.181000000000015],
          :size         [10 10],
          :critical-val 2.1009,
          :type         :two-sample,
          :upper        8.05675922207777,
          :lower        -2.536759222077789})))


(deftest one-sample-conf-inter-test
  (is (= (one-sample-confidence-interval {:mean               (mean population-one)
                                          :standard-deviation (:val (standard-deviation {:standard-deviation :sample} population-one))
                                          :size               (count population-one)
                                          :critical-val       1.8331})

         {:mean               579.0,
          :standard-deviation 65.05553183413554,
          :size               10,
          :critical-val       1.8331,
          :type               :one-sample,
          :upper              616.7112031961178,
          :lower              541.2887968038822})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;WORKSPACE

;If t-statistic is greater than critical value we can reject the null hypothesis

(one-sample-t-test {:mean               (mean population-one)
                    :standard-deviation (:val (standard-deviation {:standard-deviation :sample} population-one))
                    :hypo-mean          400
                    :size               (count population-one)})

(t-table {:dof 18 :alpha 0.05 :test :two-tail})


(one-sample-confidence-interval {:mean               (mean population-one)
                                 :standard-deviation (:val (standard-deviation {:standard-deviation :sample} population-one))
                                 :size               (count population-one)
                                 :critical-val       1.8331})


(two-sample-t-test {:two-sample-t-test :equal-variance}
                   {:mean            [(mean ballet-dancers) (mean football-players)]
                    :pooled-variance [(:val (variance {:variance :pooled} ballet-dancers)) (:val (variance {:variance :pooled} football-players))] ;TODO converge to one map
                    :size            [(count ballet-dancers) (count football-players)]})


(two-sample-t-test {:two-sample-t-test :unequal-variance}
                   {:mean            [(mean ballet-dancers) (mean football-players)]
                    :pooled-variance [(:val (variance {:variance :pooled} ballet-dancers)) (:val (variance {:variance :pooled} football-players))]
                    :size            [(count ballet-dancers) (count football-players)]})


(two-sample-confidence-interval {:mean         [(mean ballet-dancers) (mean football-players)]
                                 :variance     [(:val (variance {:variance :pooled} ballet-dancers)) (:val (variance {:variance :pooled} football-players))]
                                 :size         [(count ballet-dancers) (count football-players)]
                                 :critical-val 2.1009})

(defn null-hypothesis
  [test critical-val]
  (if (> (Math/abs (:t-statistic test)) (:critical-val critical-val))
    (assoc {} :hypothesis :reject :difference (- (:t-statistic test) (:critical-val critical-val)))
    (assoc {} :hypothesis :accept :difference (- (:t-statistic test) (:critical-val critical-val)))))

