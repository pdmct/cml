(ns cml.core.inference.t-test
  (:require [cml.core.inference.tables :refer [t-table]]))
(use 'clojure.core.matrix)

;TODO Have functions comply with dataframes


(defprotocol Test
  (one-sample [one] "One tail t-test")
  (equal-variance [two] "Two tail t-test")
  (welch [welch] "Welsch's t-test")
  (repeated-measure [rm] "Repeated measure t-test")
  (one-tail [ot] "One tail significance test")
  (two-tail [tt] "Two tail significance test"))

(defn one-sample-t-test [mean standard-deviation hypothetical-mean size]
  {:mean mean
   :standard-deviation standard-deviation
   :hypothetical-mean hypothetical-mean
   :size size
   :TTest :OneSample})

(defn equal-variance-t-test [sample-mean population-mean pooled-variance size]
  {:sample-mean sample-mean
   :population-mean population-mean
   :pooled-variance pooled-variance
   :size size
   :TTest :EqualVariance})

(defn welch-t-test [mean sample-variance size]
  {:mean mean
   :sample-variance sample-variance
   :size size
   :TTest :Welch})

(defn repeated-measure-t-test [difference-mean population-mean standard-deviation size]
  {:difference-mean difference-mean
   :population-mean population-mean
   :standard-deviation standard-deviation
   :size size
   :TTest :RepeatedMeasure})

(defrecord TTest [test]
  Test
  (one-sample [type]
    (assoc test
      :t-statistic (/ (- (:mean test)
                         (:hypothetical-mean test))
                      (/ (:standard-deviation test)
                         (Math/sqrt (:size test))))

      :dof (dec (:size test))
      :Type type))

  (equal-variance [type]
    (assoc test
      :t-statistic (/ (- (- ((:sample-mean test) 0)
                            ((:sample-mean test) 1))
                         (- ((:population-mean test) 0)
                            ((:population-mean test) 1)))
                      (Math/sqrt (* (/ (+ ((:pooled-variance test) 0)
                                          ((:pooled-variance test) 1))
                                       2)
                                    (+ (/ 1
                                          ((:size test) 0))
                                       (/ 1
                                          ((:size test) 1))))))
      :dof (- (+ ((:size test) 0)
                 ((:size test) 1)) 2)
      :Type type))

  (welch [type]
    (assoc test
      :t-statistic (/ (- ((:mean test) 0)
                         ((:mean test) 1))
                      (Math/sqrt (+ (/ ((:sample-variance test) 0)
                                       ((:size test) 0))
                                    (/ ((:sample-variance test) 1)
                                       ((:size test) 1)))))
      :dof (/ (* (+ (/ ((:sample-variance test) 0)
                       ((:size test) 0))
                    (/ ((:sample-variance test) 1)
                       ((:size test) 1)))
                 (+ (/ ((:sample-variance test) 0)
                       ((:size test) 0))
                    (/ ((:sample-variance test) 1)
                       ((:size test) 1))))
              (+ (/ (* (/ ((:sample-variance test) 0)
                          ((:size test) 0))
                       (/ ((:sample-variance test) 0)
                          ((:size test) 0)))
                    (- ((:size test) 0)
                       1))
                 (/ (* (/ ((:sample-variance test) 1)
                          ((:size test) 1))
                       (/ ((:sample-variance test) 1)
                          ((:size test) 1)))
                    (- ((:size test) 1)
                       1))))
      :Type type))

  (repeated-measure [type]
    (assoc test
      :t-statistic (/ (- (:difference-mean test)
                         (- ((:population-mean test) 0)
                            ((:population-mean test) 1)))
                      (/ (:standard-deviation test)
                         (Math/sqrt (:size test))))
      :dof (- (:size test) 1)
      :Type type)))


(defn one-tail-significance-test [dof alpha]
  {:dof dof
   :alpha alpha
   :SignificanceTest :OneTail})


(defn two-tail-significance-test [dof alpha]
  {:dof dof
   :alpha alpha
   :SignificanceTest :TwoTail})

(defrecord SignificanceTest [test]
  Test
  (one-tail [type]
    (assoc test
      :critical-value (mget t-table (dec (:dof test))
                            ({0.05 0 0.025 1 0.01 2 0.005 3 0.0025 4 0.001 5 0.0005 6}
                                             (:alpha test)))
      :Type type))

  (two-tail [type]
    (assoc test
      :critical-value (mget t-table (dec (:dof test))
                            ({0.1 0 0.05 1 0.02 2 0.01 3 0.005 4 0.002 5 0.001 6}
                              (:alpha test)))
      :Type type)))


