(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.utils.statistics :refer :all]
            [cml.core.inference.estimation.confidence :refer :all]
            [cml.core.inference.hypothesis.t-test :refer :all]
            [cml.core.utils.samples :refer :all]
            [cml.core.inference.tables :refer :all]
            [cml.core.dataset :refer :all]
            [cml.core.file :refer :all]
            [cml.core.extract :refer :all]
            [cml.core.utils :refer :all]))


(deftest one-sample-t-test-test
  (is (= (t-test {:mean               (mean population-one)
                  :standard-deviation (standard-deviation {:data population-one :mean (mean population-one) :type :sample})
                  :hypo-mean          400
                  :size               (count population-one)
                  :type               :one-sample})

         {:mean               579.0,
          :standard-deviation 65.05553183413554,
          :hypo-mean          400,
          :size               10,
          :type               :one-sample,
          :t-statistic        8.700992601418207,
          :dof                9})))


(deftest two-sample-t-test-equal-variance
  (is (= (t-test {:sample-mean     [(mean ballet-dancers) (mean football-players)]
                  :population-mean [0 0]
                  :pooled-variance [(variance {:data   ballet-dancers
                                               :size-1 (- (count ballet-dancers) 1)
                                               :mean   (mean ballet-dancers)
                                               :type   :pooled})

                                    (variance {:data   football-players
                                               :size-1 (- (count football-players) 1)
                                               :mean   (mean football-players)
                                               :type   :pooled})]
                  :size            [(count ballet-dancers) (count football-players)]
                  :type            :equal-variance})

         {:sample-mean     [87.94999999999999 85.19],
          :population-mean [0 0]
          :pooled-variance [32.382777777777775 31.181000000000015],
          :size            [10 10],
          :type            :equal-variance,
          :t-statistic     1.094722972460392,
          :dof             18})))


(deftest two-sample-t-test-unequal-variance
  (is (= (t-test {:mean            [(mean ballet-dancers) (mean football-players)]
                  :sample-variance [(variance {:data ballet-dancers
                                               :mean (mean ballet-dancers)
                                               :type :sample})
                                    (variance {:data football-players
                                               :mean (mean football-players)
                                               :type :sample})]
                  :size            [(count ballet-dancers) (count football-players)]
                  :type            :welch})

         {:mean            [87.94999999999999 85.19],
          :sample-variance [32.382777777777775 31.181000000000015],
          :size            [10 10],
          :type            :welch,
          :t-statistic     1.0947229724603922,
          :dof             17.993567997176537,})))


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


(deftest two-sample-repeated-measure-test
  (is (= (t-test {:difference-mean    (mean (difference {:sample-one after :sample-two before}))
                  :population-mean    [0 0]                 ;As with the two-sample t-test, often the quantity (µ1 − µ2) is hypothesized to be 0
                  :standard-deviation (standard-deviation {:data (difference {:sample-one after
                                                                              :sample-two before})
                                                           :mean (mean (difference {:sample-one after
                                                                                    :sample-two before}))
                                                           :type :sample})
                  :size               (/ (+ (count after) (count before)) 2)
                  :type               :repeated-measure})


         {:difference-mean    -11.0,
          :population-mean    [0 0],
          :standard-deviation 13.90443574307614,
          :size               10,
          :type               :repeated-measure,
          :t-statistic        -2.5017235438103813,
          :dof                9})))


(deftest one-tail-test-test
  (is (= (one-tail (t-test {:mean               (mean population-one)
                            :standard-deviation (standard-deviation {:data population-one :mean (mean population-one) :type :sample})
                            :hypo-mean          400
                            :size               (count population-one)
                            :type               :one-sample
                            :alpha              0.05}))
         {:mean               579.0,
          :standard-deviation 65.05553183413554,
          :hypo-mean          400,
          :size               10,
          :type               :one-sample,
          :alpha              0.05,
          :t-statistic        8.700992601418207,
          :dof                9,
          :critical-value     1.8331})))


(deftest two-tail-test-test
  (is (= (two-tail (t-test {:mean               (mean population-one)
                            :standard-deviation (standard-deviation {:data population-one :mean (mean population-one) :type :sample})
                            :hypo-mean          400
                            :size               (count population-one)
                            :type               :one-sample
                            :alpha              0.05}))

         {:mean               579.0,
          :standard-deviation 65.05553183413554,
          :hypo-mean          400,
          :size               10,
          :type               :one-sample,
          :alpha              0.05,
          :t-statistic        8.700992601418207,
          :dof                9,
          :critical-value     2.2621})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dataset "/Users/gra11/IdeaProjects/cml/resources/datasets/adult/adult.data")

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








(defmulti area :Shape)
(defn rect [wd ht] {:Shape :Rect :wd wd :ht ht})
(defn circle [radius] {:Shape :Circle :radius radius})

(defmethod area :Rect [r]
  (* (:wd r) (:ht r)))

(defmethod area :Circle [c]
  (* (. Math PI) (* (:radius c) (:radius c))))
(defmethod area :default [x] :oops)

(rect 3 6)

(area {:Shape :Rect, :wd 4, :ht 13})