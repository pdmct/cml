(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.core.correlation :refer :all]
            [cml.core.utils.stats :refer :all]
            [cml.core.inference.estimation.confidence :refer :all]
            [cml.core.inference.hypothesis.critical-value :refer :all]
            [cml.core.sample :refer :all]
            [cml.core.utils.samples :refer :all]
            [cml.core.inference.hypothesis.test :refer :all]
            [cml.core.dataset :refer :all]
            [cml.core.transform :refer :all]))


(def sample {:x-axis (deviation-score mean [490 500 530 550 580 590 600 600 650 700])
             :y-axis (deviation-score mean [560 500 510 600 600 620 550 630 650 750])})


(deftest pearson-correlation-test
  (is (= (pearson-correlation sample) 0.8702620996632292)))


(deftest sig-pearson-correlation-test
  (is (= (significance (pearson-correlation sample) 8))))


(deftest coefficient-determination-pearson-test
  (is (= (coefficient-determination (pearson-correlation sample)) 0.7573561221102523)))


(deftest one-sample-t-test-test
  (is (= (t-test {:mean               (mean population-one)
                  :standard-deviation (standard-deviation {:data population-one :type :sample})
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
                  :pooled-variance [(variance {:data ballet-dancers :type :pooled}) (variance {:data football-players :type :pooled})]
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
                  :sample-variance [(variance {:data ballet-dancers :type :sample}) (variance {:data football-players :type :sample})]
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
                               :standard-deviation (standard-deviation {:data population-one :type :sample})
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
                               :variance     [(variance {:data ballet-dancers :type :pooled}) (variance {:data football-players :type :pooled})]
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
  (is (= (t-test {:difference-mean    (mean (difference {:s1 after :s2 before}))
                  :population-mean    [0 0]                 ;As with the two-sample t-test, often the quantity (µ1 − µ2) is hypothesized to be 0
                  :standard-deviation (standard-deviation {:data (difference {:s1 after :s2 before}) :type :sample})
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
                            :standard-deviation (standard-deviation {:data population-one :type :sample})
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
                            :standard-deviation (standard-deviation {:data population-one :type :sample})
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


(deftest data-frame-test-group
  (is (= (group-by :isstre (data-frame "/Users/gra11/IdeaProjects/cml/resources/datasets/balloons/adult-stretch.data"
                                      #"," [:color :size :isstre :human :type]))
        {"STRETCH" [{:color "YELLOW", :size "SMALL", :isstre "STRETCH", :human "ADULT", :type "T"}
                    {:color "YELLOW", :size "SMALL", :isstre "STRETCH", :human "CHILD", :type "T"}
                    {:color "YELLOW", :size "LARGE", :isstre "STRETCH", :human "ADULT", :type "T"}
                    {:color "YELLOW", :size "LARGE", :isstre "STRETCH", :human "CHILD", :type "T"}
                    {:color "PURPLE", :size "SMALL", :isstre "STRETCH", :human "ADULT", :type "T"}
                    {:color "PURPLE", :size "SMALL", :isstre "STRETCH", :human "CHILD", :type "T"}
                    {:color "PURPLE", :size "LARGE", :isstre "STRETCH", :human "ADULT", :type "T"}
                    {:color "PURPLE", :size "LARGE", :isstre "STRETCH", :human "CHILD", :type "T"}],
         "DIP"     [{:color "YELLOW", :size "SMALL", :isstre "DIP", :human "ADULT", :type "T"}
                    {:color "YELLOW", :size "SMALL", :isstre "DIP", :human "CHILD", :type "F"}
                    {:color "YELLOW", :size "SMALL", :isstre "DIP", :human "CHILD", :type "F"}
                    {:color "YELLOW", :size "LARGE", :isstre "DIP", :human "ADULT", :type "T"}
                    {:color "YELLOW", :size "LARGE", :isstre "DIP", :human "CHILD", :type "F"}
                    {:color "YELLOW", :size "LARGE", :isstre "DIP", :human "CHILD", :type "F"}
                    {:color "PURPLE", :size "SMALL", :isstre "DIP", :human "ADULT", :type "T"}
                    {:color "PURPLE", :size "SMALL", :isstre "DIP", :human "CHILD", :type "F"}
                    {:color "PURPLE", :size "SMALL", :isstre "DIP", :human "CHILD", :type "F"}
                    {:color "PURPLE", :size "LARGE", :isstre "DIP", :human "ADULT", :type "T"}
                    {:color "PURPLE", :size "LARGE", :isstre "DIP", :human "CHILD", :type "F"}
                    {:color "PURPLE", :size "LARGE", :isstre "DIP", :human "CHILD", :type "F"}]})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(map #(xform-all %
                 (fn [x] (if (clojure.string/starts-with? x "B") (clojure.string/upper-case x) x))
                 clojure.string/trim)
     (data-frame dataset
                 #","
                 [:age :department :salary
                  :degree :study-time :marital-status
                  :job :family-status :race
                  :gender :n1 :n2 :n3 :country :salary-range]))

#_(pvalues (partial + 2) )
#_((comp (fn [x] (update x :department clojure.string/upper-case))
       (fn [x] (update x :foo clojure.string/upper-case))) {:department "fff" :foo "eee"})

