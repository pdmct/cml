(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.utils.statistics :refer [mean standard-deviation variance difference]]
            [cml.core.inference.estimate.confidence-interval :refer [confidence-interval]]
            [cml.core.utils.samples :refer :all]
            [cml.core.inference.tables :refer :all]
            [cml.core.dataset :refer :all]
            [cml.core.file :refer :all]
            [cml.core.extract :refer :all]
            [cml.core.utils :refer :all]
            [cml.core.inference.test.t-test :refer [one-sample equal-variance welch repeated-measure one-sample-t-test equal-variance-t-test
                                                    welch-t-test repeated-measure-t-test one-tail-significance-test two-tail-significance-test
                                                    one-tail two-tail]]
            [cml.core.inference.estimate.confidence-interval :refer [one-sample-confidence-interval one-sample-confidence-interval-test
                                                                     two-sample-confidence-interval two-sample-confidence-interval-test]])
  (:import [cml.core.inference.test.t_test TTest SignificanceTest]
           [cml.core.inference.estimate.confidence_interval ConfidenceInterval]))


(deftest one-sample-t-test-test
  (is (= (one-sample (TTest.
                       (one-sample-t-test (mean population-one)
                                          (standard-deviation {:data population-one :mean (mean population-one) :type :sample})
                                          400
                                          (count population-one))))

         #cml.core.inference.test.t_test.TTest{:ttest {:mean 579.0,
                                                 :standard-deviation 65.05553183413554,
                                                 :hypothetical-mean 400,
                                                 :size 10,
                                                 :Test :OneSample},
                                          :t-statistic 8.700992601418207,
                                          :dof 9})))


(deftest two-sample-t-test-equal-variance
  (is (= (equal-variance (TTest. (equal-variance-t-test
                                    [(mean ballet-dancers) (mean football-players)]
                                    [0 0]
                                    [(variance {:data   ballet-dancers
                                                :size-1 (- (count ballet-dancers) 1)
                                                :mean   (mean ballet-dancers)
                                                :type   :pooled})

                                     (variance {:data   football-players
                                                :size-1 (- (count football-players) 1)
                                                :mean   (mean football-players)
                                                :type   :pooled})]
                                    [(count ballet-dancers) (count football-players)])))

         #cml.core.inference.test.t_test.TTest{:ttest {:sample-mean [87.94999999999999 85.19],
                                                 :population-mean [0 0],
                                                 :pooled-variance [32.382777777777775 31.181000000000015],
                                                 :size [10 10],
                                                 :Test :EqualVariance},
                                          :t-statistic 1.094722972460392,
                                          :dof 18})))

(deftest two-sample-t-test-unequal-variance
  (is (= (welch (TTest. (welch-t-test
                           [(mean ballet-dancers) (mean football-players)]
                           [(variance {:data ballet-dancers
                                       :mean (mean ballet-dancers)
                                       :type :sample})
                            (variance {:data football-players
                                       :mean (mean football-players)
                                       :type :sample})]
                           [(count ballet-dancers) (count football-players)])))

         #cml.core.inference.test.t_test.TTest{:ttest {:mean [87.94999999999999 85.19],
                                                 :sample-variance [32.382777777777775 31.181000000000015],
                                                 :size [10 10],
                                                 :Test :Welch},
                                          :t-statistic 1.0947229724603922,
                                          :dof 17.993567997176537})))


(deftest two-sample-repeated-measure-test
  (is (= (repeated-measure (TTest. (repeated-measure-t-test
                                      (mean (difference {:sample-one after :sample-two before}))
                                      [0 0] ;As with the two-sample t-test, often the quantity (µ1 − µ2) is hypothesized to be 0
                                      (standard-deviation {:data (difference {:sample-one after
                                                                              :sample-two before})
                                                           :mean (mean (difference {:sample-one after
                                                                                    :sample-two before}))
                                                           :type :sample})
                                      (/ (+ (count after) (count before)) 2))))


         #cml.core.inference.test.t_test.TTest{:ttest {:difference-mean -11.0,
                                                 :population-mean [0 0],
                                                 :standard-deviation 13.90443574307614,
                                                 :size 10,
                                                 :Test :RepeatedMeasure},
                                          :t-statistic -2.5017235438103813,
                                          :dof 9})))


(deftest one-sample-conf-inter-test
  (is (= (one-sample-confidence-interval-test
           (ConfidenceInterval.
             (one-sample-confidence-interval
               (mean population-one)
               (standard-deviation {:data population-one :mean (mean population-one) :type :sample})
               (count population-one)
               1.8331)))

         #cml.core.inference.estimate.confidence_interval.ConfidenceInterval{:confidence-interval {:mean 579.0,
                                                                                                   :standard-deviation 65.05553183413554,
                                                                                                   :size 10,
                                                                                                   :critical-value 1.8331,
                                                                                                   :Estimate :OneSample},
                                                                             :upper 616.7112031961178,
                                                                             :lower 541.2887968038822})))


(deftest two-sample-confidence-interval-test-test
  (is (= (two-sample-confidence-interval-test
           (ConfidenceInterval.
             (two-sample-confidence-interval
               [(mean ballet-dancers) (mean football-players)]
               [(variance {:data   ballet-dancers
                           :mean   (mean ballet-dancers)
                           :size-1 (- (count ballet-dancers) 1)
                           :type   :pooled})
                (variance {:data   football-players
                           :mean   (mean football-players)
                           :size-1 (- (count football-players) 1)
                           :type   :pooled})]
               [(count ballet-dancers) (count football-players)]
               2.1009)))

         #cml.core.inference.estimate.confidence_interval.ConfidenceInterval{:confidence-interval {:mean [87.94999999999999
                                                                                                          85.19],
                                                                                                   :variance [32.382777777777775
                                                                                                              31.181000000000015],
                                                                                                   :size [10 10],
                                                                                                   :critical-value 2.1009,
                                                                                                   :Estimate :TwoSample},
                                                                             :upper 8.05675922207777,
                                                                             :lower -2.536759222077789})))


(deftest one-tail-significance-test-test
  (is (= (one-tail (SignificanceTest. (one-tail-significance-test  9 0.05)))
         #cml.core.inference.test.t_test.SignificanceTest{:significance-test {:dof 9, :alpha 0.05, :Test :OneTail},
                                                     :critical-value 1.8331})))


(deftest two-tail-significance-test-test
  (is (= (two-tail (SignificanceTest. (two-tail-significance-test  9 0.05)))
         #cml.core.inference.test.t_test.SignificanceTest{:significance-test {:dof 9, :alpha 0.05, :Test :TwoTail},
                                                     :critical-value 2.2621})))

;WORKSPACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dataset "/Users/gregadebesin/IdeaProjects/cml/resources/datasets/adult/adult.data")

(data-frame {:column-names [:age :department :salary
                            :degree :study-time :marital-status
                            :job :family-status :race
                            :gender :n1 :n2 :n3 :country :salary-range]
             :delimiter    ","
             :file-path    dataset
             :type         :csv/read
             :return '()})


(data-frame {:column-names [:age :department :salary
                            :degree :study-time :marital-status
                            :job :family-status :race
                            :gender :n1 :n2 :n3 :country :salary-range]
             :delimiter    ","
             :file-path    dataset
             :type         :csv/read
             :xform        (comp clojure.string/upper-case
                                 #(clojure.string/replace % #" " ""))
             :return       []})

