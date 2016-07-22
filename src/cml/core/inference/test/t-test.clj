(ns cml.core.inference.test.t-test
  (:require [cml.core.inference.tables :refer [t-table]]))
(use 'clojure.core.matrix)

;TODO Have functions comply with dataframes
;TODO have in out map

(defprotocol Test
  (one-sample-test [one] "One tail t-test")
  (equal-variance-test [two] "Two tail t-test")
  (welch-test [welch] "Welsch's t-test")
  (repeated-measure-test [rm] "Repeated measure t-test")
  (one-tail-test [ot] "One tail significance test")
  (two-tail-test [tt] "Two tail significance test"))

(defn one-sample-t-test [mean standard-deviation hypothetical-mean size]
  {:mean mean
   :standard-deviation standard-deviation
   :hypothetical-mean hypothetical-mean
   :size size
   :Test :OneSample})

(defn equal-variance-t-test [sample-mean population-mean pooled-variance size]
  {:sample-mean sample-mean
   :population-mean population-mean
   :pooled-variance pooled-variance
   :size size
   :Test :EqualVariance})

(defn welch-t-test [mean sample-variance size]
  {:mean mean
   :sample-variance sample-variance
   :size size
   :Test :Welch})

(defn repeated-measure-t-test [difference-mean population-mean standard-deviation size]
  {:difference-mean difference-mean
   :population-mean population-mean
   :standard-deviation standard-deviation
   :size size
   :Test :RepeatedMeasure})

(defrecord TTest [ttest]
  Test
  (one-sample-test [type]
    (assoc type
      :t-statistic (/ (- (:mean ttest)
                         (:hypothetical-mean ttest))
                      (/ (:standard-deviation ttest)
                         (Math/sqrt (:size ttest))))

      :dof (dec (:size ttest))))

  (equal-variance-test [type]
    (assoc type
      :t-statistic (/ (- (- ((:sample-mean ttest) 0)
                            ((:sample-mean ttest) 1))
                         (- ((:population-mean ttest) 0)
                            ((:population-mean ttest) 1)))
                      (Math/sqrt (* (/ (+ ((:pooled-variance ttest) 0)
                                          ((:pooled-variance ttest) 1))
                                       2)
                                    (+ (/ 1
                                          ((:size ttest) 0))
                                       (/ 1
                                          ((:size ttest) 1))))))
      :dof (- (+ ((:size ttest) 0)
                 ((:size ttest) 1)) 2)))

  (welch-test [type]
    (assoc type
      :t-statistic (/ (- ((:mean ttest) 0)
                         ((:mean ttest) 1))
                      (Math/sqrt (+ (/ ((:sample-variance ttest) 0)
                                       ((:size ttest) 0))
                                    (/ ((:sample-variance ttest) 1)
                                       ((:size ttest) 1)))))
      :dof (/ (* (+ (/ ((:sample-variance ttest) 0)
                       ((:size ttest) 0))
                    (/ ((:sample-variance ttest) 1)
                       ((:size ttest) 1)))
                 (+ (/ ((:sample-variance ttest) 0)
                       ((:size ttest) 0))
                    (/ ((:sample-variance ttest) 1)
                       ((:size ttest) 1))))
              (+ (/ (* (/ ((:sample-variance ttest) 0)
                          ((:size ttest) 0))
                       (/ ((:sample-variance ttest) 0)
                          ((:size ttest) 0)))
                    (- ((:size ttest) 0)
                       1))
                 (/ (* (/ ((:sample-variance ttest) 1)
                          ((:size ttest) 1))
                       (/ ((:sample-variance ttest) 1)
                          ((:size ttest) 1)))
                    (- ((:size ttest) 1)
                       1))))))

  (repeated-measure-test [type]
    (assoc type
      :t-statistic (/ (- (:difference-mean ttest)
                         (- ((:population-mean ttest) 0)
                            ((:population-mean ttest) 1)))
                      (/ (:standard-deviation ttest)
                         (Math/sqrt (:size ttest))))
      :dof (- (:size ttest) 1))))


;TODO Move significance test out so comply with protocol naming convention / file structure
(defn one-tail-significance-test [dof alpha]
  {:dof dof
   :alpha alpha
   :Test :OneTail})


(defn two-tail-significance-test [dof alpha]
  {:dof dof
   :alpha alpha
   :Test :TwoTail})

(defrecord SignificanceTest [significance-test]
  Test
  (one-tail-test [type]
    (assoc type
      :critical-value (mget t-table (dec (:dof significance-test))
                            ({0.05 0 0.025 1 0.01 2 0.005 3 0.0025 4 0.001 5 0.0005 6}
                                             (:alpha significance-test)))))

  (two-tail-test [type]
    (assoc type
      :critical-value (mget t-table (dec (:dof significance-test))
                            ({0.1 0 0.05 1 0.02 2 0.01 3 0.005 4 0.002 5 0.001 6}
                              (:alpha significance-test))))))


