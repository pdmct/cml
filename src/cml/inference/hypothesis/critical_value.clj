(ns cml.inference.hypothesis.critical-value
  (:require [cml.inference.tables :refer [t-table]]))
(use 'clojure.core.matrix)

;TODO Have functions comply with dataframes

;TODO move into file named test ie cml.core.inference.test

(defprotocol Test
  (t-test [tt] "T-test")
  (significance [s] "Significance test"))

(defrecord Dependant [sample-mean sample-standard-deviation sample-hypothetical-mean sample-size]
  Test

  (t-test [type]
    (assoc type
      :t-statistic (/ (- sample-mean
                         sample-hypothetical-mean)
                      (/ sample-standard-deviation
                         (Math/sqrt sample-size)))
      :dof (dec sample-size))))


(defrecord EqualVariance [mean population-mean pooled-variance size]
  Test

  (t-test [type]
    (assoc type
      :t-statistic (/ (- (- (first mean)
                            (second mean))
                         (- (first population-mean)
                            (second population-mean)))
                      (Math/sqrt (* (/ (+ (first pooled-variance)
                                          (second pooled-variance)) 2)
                                    (+ (/ 1 (first size))
                                       (/ 1 (second size))))))
      :dof (- (+ (first size)
                 (second size)) 2))))


(defrecord Welch [mean sample-variance size]                ;TODO destructure
  Test

  (t-test [type]
    (assoc type
      :t-statistic (/ (- (first mean)
                         (second mean))
                      (Math/sqrt (+ (/ (first sample-variance)
                                       (first size))
                                    (/ (second sample-variance)
                                       (second size)))))
      :dof (/ (* (+ (/ (first sample-variance)
                       (first size))
                    (/ (second sample-variance)
                       (second size)))
                 (+ (/ (first sample-variance)
                       (first size))
                    (/ (second sample-variance)
                       (second size))))
              (+ (/ (* (/ (first sample-variance)
                          (first size))
                       (/ (first sample-variance)
                          (first size)))
                    (- (first size)
                       1))
                 (/ (* (/ (second sample-variance)
                          (second size))
                       (/ (second sample-variance)
                          (second size)))
                    (- (second size) 1)))))))


(defrecord RepeatedMeasure [difference-mean population-mean standard-deviation size]
  Test

  (t-test [type]
    (assoc type
      :t-statistic (/ (- difference-mean
                         (- (first population-mean)
                            (second population-mean)))
                      (/ standard-deviation
                         (Math/sqrt size)))
      :dof (- size 1))))


(defrecord OneTail [dof alpha]
  Test

  (significance [type]
    (assoc type
      :critical-value
      (mget t-table (dec dof)
            ({0.05 0 0.025 1 0.01 2 0.005 3 0.0025 4 0.001 5 0.0005 6}
              alpha)))))


(defrecord TwoTail [dof alpha]
  Test

  (significance [type]
    (assoc type
      :critical-value (mget t-table (dec dof)
                            ({0.1 0 0.05 1 0.02 2 0.01 3 0.005 4 0.002 5 0.001 6}
                              alpha)))))


