(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.utils.samples :refer :all]
            [cml.inference.tables :refer :all]
            [cml.dataset :refer :all]
            [cml.extract :refer :all]
            [cml.utils :refer :all]
            [cml.statistics.central-tendancy :refer [mean mean-1 difference]]
            [cml.statistics.variation :refer [standard-deviation variance]]
            [cml.inference.hypothesis.critical-value :refer [t-test significance]]
            [cml.inference.estimate.confidence-interval :refer [confidence-interval]])
  (:import [cml.inference.hypothesis.critical_value Dependant EqualVariance Welch RepeatedMeasure OneTail TwoTail]
           [cml.inference.estimate.confidence_interval OneSample TwoSample]
           [cml.statistics.variation Sample Population Pooled]))


(deftest one-sample-t-test-test
  (is (= (t-test (Dependant.
                   (mean population-one)
                   (:standard-deviation (standard-deviation (Sample. (mean population-one) population-one)))
                   400
                   (count population-one)))

         #cml.inference.hypothesis.critical_value.Dependant{:sample-mean 579.0,
                                                           :sample-standard-deviation 65.05553183413554,
                                                   :sample-hypothetical-mean 400,
                                                   :sample-size 10,
                                                   :t-statistic 8.700992601418207,
                                                   :dof 9})))


(deftest two-sample-t-test-equal-variance
  (is (= (t-test (EqualVariance.
                   [(mean ballet-dancers) (mean football-players)]
                   [0 0]
                   [(:variance (variance (Pooled. (mean ballet-dancers) ballet-dancers (- (count ballet-dancers) 1))))
                    (:variance (variance (Pooled. (mean football-players) football-players (- (count football-players) 1))))]
                   [(count ballet-dancers) (count football-players)]))

         #cml.inference.hypothesis.critical_value.EqualVariance{:mean [87.94999999999999 85.19],
                                                       :population-mean [0 0],
                                                       :pooled-variance [32.382777777777775 31.181000000000015],
                                                       :size [10 10],
                                                       :t-statistic 1.094722972460392,
                                                       :dof 18})))

(deftest two-sample-t-test-unequal-variance
  (is (= (t-test (Welch. [(mean ballet-dancers) (mean football-players)]
                         [(:variance
                            (variance (Sample. (mean ballet-dancers)
                                               ballet-dancers)))
                          (:variance
                            (variance (Sample. (mean football-players)
                                               football-players)))]

                         [(count ballet-dancers) (count football-players)]))

         #cml.inference.hypothesis.critical_value.Welch{:mean [87.94999999999999 85.19],
                                               :sample-variance [32.382777777777775 31.181000000000015],
                                               :size [10 10],
                                               :t-statistic 1.0947229724603922,
                                               :dof 17.993567997176537})))


(deftest two-sample-repeated-measure-test
  (is (= (t-test
           (RepeatedMeasure.
             (mean (difference [after before]))
             [0 0]                                   ;As with the two-sample t-test, often the quantity (µ1 − µ2) is hypothesized to be 0
             (:standard-deviation (standard-deviation (Sample. (mean (difference [after before]))
                                                               (difference [after before]))))
             (/ (+ (count after) (count before)) 2)))
         #cml.inference.hypothesis.critical_value.RepeatedMeasure{:difference-mean -11.0,
                                                         :population-mean [0 0],
                                                         :standard-deviation 13.90443574307614,
                                                         :size 10,
                                                         :t-statistic -2.5017235438103813,
                                                         :dof 9})))


(deftest one-sample-conf-inter-test
  (is (= (confidence-interval
           (OneSample.
             (mean population-one)
             (:standard-deviation (standard-deviation (Sample. (mean population-one) population-one)))
             (count population-one)
             1.8331))

         #cml.inference.estimate.confidence_interval.OneSample{:sample-mean 579.0,
                                                                    :sample-standard-deviation 65.05553183413554,
                                                                    :sample-size 10,
                                                                    :critical-value 1.8331,
                                                                    :upper 616.7112031961178,
                                                                    :lower 541.2887968038822})))


(deftest two-sample-confidence-interval-test-test
  (is (= (confidence-interval
           (TwoSample.
             [(mean ballet-dancers) (mean football-players)]
             [(:variance (variance (Sample. (mean ballet-dancers) ballet-dancers)))
              (:variance (variance (Sample. (mean football-players) football-players)))]
             [(count ballet-dancers) (count football-players)]
             2.1009))

         #cml.inference.estimate.confidence_interval.TwoSample{:sample-mean [87.94999999999999 85.19],
                                                                    :sample-variance [32.382777777777775 31.181000000000015],
                                                                    :sample-size [10 10],
                                                                    :critical-value 2.1009,
                                                                    :upper 8.05675922207777,
                                                                    :lower -2.536759222077789})))


(deftest one-tail-significance-test-test
  (is (= (significance (OneTail. 9 0.05))
         #cml.inference.hypothesis.critical_value.OneTail{:dof 9, :alpha 0.05, :critical-value 1.8331})))


(deftest two-tail-significance-test-test
  (is (= (significance (TwoTail. 9 0.05))
         #cml.inference.hypothesis.critical_value.TwoTail{:dof 9, :alpha 0.05, :critical-value 2.2621})))

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

; 0.121288
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

;TODO piece together high level API as below stored in core ns

(time
  (pvalues
    (confidence-interval
      (OneSample.
        (mean (range 1 1000000))
        (:standard-deviation (standard-deviation (Sample. (mean (range 1 1000000)) (range 1 1000000))))
        (count (range 1 1000000))
        1.8331))))

;TODO add a p-os-conf-seq which pmaps this fn accross a sequence of data sets and also uses a transducer?

#_(defn os-conf [{:keys [data critical-value]}]
  (confidence-interval
    (OneSample.
      (mean data)
      (variation (sample (mean data) data))
      (count data)
      critical-value)))

#_(defn p-os-conf [{:keys [data critical-value]}]
  (pvalues (os-conf {:data data :critical-value critical-value})))


