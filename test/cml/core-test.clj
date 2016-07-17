(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.utils.statistics :refer [mean standard-deviation variance difference]]
            [cml.core.inference.estimation.confidence :refer [confidence-interval]]
            [cml.core.utils.samples :refer :all]
            [cml.core.inference.tables :refer :all]
            [cml.core.dataset :refer :all]
            [cml.core.file :refer :all]
            [cml.core.extract :refer :all]
            [cml.core.utils :refer :all]
            [cml.core.inference.t-test :refer [one-sample equal-variance welch repeated-measure one-sample-t-test equal-variance-t-test welch-t-test repeated-measure-t-test]])
  (:import [cml.core.inference.t_test TTest]))


(deftest one-sample-t-test-test
  (is (= (one-sample (TTest.
                       (one-sample-t-test (mean population-one)
                                          (standard-deviation {:data population-one :mean (mean population-one) :type :sample})
                                          400
                                          (count population-one))))

         {:mean 579.0,
          :standard-deviation 65.05553183413554,
          :hypothetical-mean 400,
          :size 10,
          :t-statistic 8.700992601418207,
          :dof 9,
          :TTest :OneSample})))


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

         {:sample-mean [87.94999999999999 85.19],
          :population-mean [0 0],
          :pooled-variance [32.382777777777775 31.181000000000015],
          :size [10 10],
          :t-statistic 1.094722972460392,
          :dof 18,
          :TTest :EqualVariance})))

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

         {:mean [87.94999999999999 85.19],
          :sample-variance [32.382777777777775 31.181000000000015],
          :size [10 10],
          :t-statistic 1.0947229724603922,
          :dof 17.993567997176537,
          :TTest :Welch})))


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


         {:difference-mean -11.0,
          :population-mean [0 0],
          :standard-deviation 13.90443574307614,
          :size 10,
          :t-statistic -2.5017235438103813,
          :dof 9,
          :TTest :RepeatedMeasure})))


(deftest one-sample-conf-inter-test
  (is (= (confidence-interval {:mean               (mean population-one)
                               :standard-deviation (standard-deviation {:data population-one :mean (mean population-one) :type :sample})
                               :size               (count population-one)
                               :critical-val       1.8331
                               :type               :one-sample})

         {:mean               579.0,
          :standard-deviation 65.05553183413554,
          :size               10,
          :critical-val       1.8331,
          :type               :one-sample,
          :upper              616.7112031961178,
          :lower              541.2887968038822})))


(deftest two-sample-confidence-interval-test
  (is (= (confidence-interval {:mean         [(mean ballet-dancers) (mean football-players)]
                               :variance     [(variance {:data   ballet-dancers
                                                         :mean   (mean ballet-dancers)
                                                         :size-1 (- (count ballet-dancers) 1)
                                                         :type   :pooled})
                                              (variance {:data   football-players
                                                         :mean   (mean football-players)
                                                         :size-1 (- (count football-players) 1)
                                                         :type   :pooled})]
                               :size         [(count ballet-dancers) (count football-players)]
                               :critical-val 2.1009
                               :type         :two-sample})

         {:mean         [87.94999999999999 85.19],
          :variance     [32.382777777777775 31.181000000000015],
          :size         [10 10],
          :critical-val 2.1009,
          :type         :two-sample,
          :upper        8.05675922207777,
          :lower        -2.536759222077789})))

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

