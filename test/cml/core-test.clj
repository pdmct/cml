(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.utils.samples :refer :all]
            [cml.dataset :refer [data-frame]]
            [cml.extract :refer [file-lines]]
            [cml.utils :refer [zip]]
            [cml.core.numerical.estimate :refer [one-sample-conf-inter two-sample-conf-inter]]
            [cml.core.numerical.hypothesis.test :refer [one-sample-ttest equal-var-ttest welch-ttest rep-measure-ttest one-tail-sig two-tail-sig]]))


(deftest one-sample-t-test-test
  (is (= (one-sample-ttest {:sample population-one :h-mean 400})
         #cml.numerical.hypothesis.test.OneSample{:sample-mean                         579.0,
                                                            :sample-standard-deviation 65.05553183413554,
                                                            :sample-hypothetical-mean  400,
                                                            :sample-size               10,
                                                            :t-statistic               8.700992601418207,
                                                            :dof                       9})))


(deftest two-sample-t-test-equal-variance
  (is (= (equal-var-ttest {:sample [ballet-dancers football-players] :hp-mean [0 0]})
         #cml.numerical.hypothesis.test.EqualVariance{:mean                      (87.94999999999999 85.19),
                                                                :population-mean (0.0 0.0),
                                                                :pooled-variance (32.382777777777775 31.181000000000015),
                                                                :size            (10 10),
                                                                :t-statistic     1.094722972460392,
                                                                :dof             18})))

(deftest two-sample-t-test-unequal-variance
  (is (= (welch-ttest {:sample [ballet-dancers football-players]})
         #cml.numerical.hypothesis.test.Welch{:mean            (87.94999999999999 85.19),
                                              :sample-variance (32.382777777777775 31.181000000000015),
                                              :size            (10 10),
                                              :t-statistic     1.0947229724603922,
                                              :dof             17.993567997176537})))


(deftest two-sample-repeated-measure-test
  (is (= (rep-measure-ttest {:population [after before] :hp-mean [0 0]})
         #cml.numerical.hypothesis.test.RepeatedMeasure{:difference-mean              -11.0,
                                                                  :population-mean    (0.0 0.0),
                                                                  :standard-deviation 13.90443574307614,
                                                                  :size               10,
                                                                  :t-statistic        -2.5017235438103813,
                                                                  :dof                9})))


(deftest one-sample-conf-inter-test
  (is (= (one-sample-conf-inter {:sample population-one :critical-value 1.8331})
         #cml.numerical.estimate.OneSample{:sample-mean                                   579.0,
                                                               :sample-standard-deviation 65.05553183413554,
                                                               :sample-size               10,
                                                               :critical-value            1.8331,
                                                               :upper                     616.7112031961178,
                                                               :lower                     541.2887968038822})))


(deftest two-sample-confidence-interval-test-test
  (is (= (two-sample-conf-inter {:sample [ballet-dancers football-players] :critical-value 2.1009})
         #cml.numerical.estimate.TwoSample{:sample-mean                         (87.94999999999999 85.19),
                                                               :sample-variance (32.382777777777775 31.181000000000015),
                                                               :sample-size     (10 10),
                                                               :critical-value  2.1009,
                                                               :upper           8.05675922207777,
                                                               :lower           -2.536759222077789})))


(deftest one-tail-significance-test-test
  (is (= (one-tail-sig {:dof 9 :alpha 0.05})
         #cml.numerical.hypothesis.test.OneTail{:dof 9, :alpha 0.05, :critical-value 1.8331})))


(deftest two-tail-significance-test-test
  (is (= (two-tail-sig {:dof 9 :alpha 0.05})
         #cml.numerical.hypothesis.test.TwoTail{:dof 9, :alpha 0.05, :critical-value 2.2621})))

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

