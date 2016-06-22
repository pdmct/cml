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
            [cml.core.transform :refer :all]
            [cml.core.file :refer :all]
            [cml.core.extract :refer :all]))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO write tests around below functionality


#_(map (fn [x] (tokenize-line-type #"," x [:age :integer :department :string :salary :long
                                           :degree :string :study-time :integer :marital-status :string
                                           :job :string :family-status :string :race :string
                                           :gender :string :n1 :integer :n2 :integer :n3 :integer
                                           :country :string :salary-range :string]
                                   #(clojure.string/replace % #" " "")))
       (file-lines dataset))


#_(eduction
    (map (comp
           (fn [y]
             (transform-by-key y :race clojure.string/upper-case))
           (fn [y]
             (transform-by-key y :degree clojure.string/upper-case))
           (fn [y]
             (transform-by-key y :age (fn [x] (Integer/parseInt x))))))
    (map (fn [x] (tokenize-line
                   #","
                   x
                   [:age :department :salary
                    :degree :study-time :marital-status
                    :job :family-status :race
                    :gender :n1 :n2 :n3 :country :salary-range]
                   #(clojure.string/replace % #" " "")))
         (file-lines dataset)))


#_(defn zipmap-types [keys vals]                            ;TODO Remove this function and build function for specificallvy parsing data frame values
  (loop [map (transient {})
         ks (seq keys)
         vs (seq vals)]
    (if (and (apply hash-map
                    ks)
             vs)
      (recur (assoc! map
                     (first ks)
                     (cond (= (second ks)
                              :string)
                           (first vs)
                           (=  (second ks)
                               :integer)
                           (Integer/parseInt (first vs))
                           (= (second ks)
                              :long)
                           (Long/parseLong (first vs))
                           (= (second ks)
                              :double)
                           (Double/parseDouble (first vs))
                           (= (second ks)
                              :character)
                           (.charAt (first vs) 0)
                           :else (first vs)))
             (drop 2 ks)
             (next vs)) (persistent! map))))


