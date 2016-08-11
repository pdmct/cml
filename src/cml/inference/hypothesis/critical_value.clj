(ns cml.inference.hypothesis.critical-value
  (:require [cml.inference.tables :refer [t-table]]))
(use 'clojure.core.matrix)

;TODO Have functions comply with dataframes


(defprotocol Test
  (t-test [tt] "T-test")
  (significance [s] "Significance test"))

(defrecord OneSample [sample-mean sample-standard-deviation sample-hypothetical-mean sample-size]
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
    (let [[mean-one mean-two] mean
          [population-mean-one population-mean-two] population-mean
          [pooled-variance-one pooled-variance-two] pooled-variance
          [size-one size-two] size]
      (assoc type
        :t-statistic (/ (- (- mean-one mean-two)
                           (- population-mean-one population-mean-two))
                        (Math/sqrt (* (/ (+ pooled-variance-one pooled-variance-two) 2)
                                      (+ (/ 1 size-one) (/ 1 size-two)))))
        :dof (- (+ size-one size-two) 2)))))


(defrecord Welch [mean sample-variance size]
  Test

  (t-test [type]
    (let [[mean-one mean-two] mean
          [sample-variance-one sample-variance-two] sample-variance
          [size-one size-two] size]
      (assoc type
        :t-statistic (/ (- mean-one mean-two)
                        (Math/sqrt (+ (/ sample-variance-one size-one)
                                      (/ sample-variance-two size-two))))
        :dof (/ (* (+ (/ sample-variance-one size-one)
                      (/ sample-variance-two size-two))
                   (+ (/ sample-variance-one size-one)
                      (/ sample-variance-two size-two)))
                (+ (/ (* (/ sample-variance-one size-one)
                         (/ sample-variance-one size-one))
                      (- size-one 1))
                   (/ (* (/ sample-variance-two size-two)
                         (/ sample-variance-two size-two))
                      (- size-two 1))))))))


(defrecord RepeatedMeasure [difference-mean population-mean standard-deviation size]
  Test

  (t-test [type]
    (let [[population-mean-one population-mean-two] population-mean]
      (assoc type
        :t-statistic (/ (- difference-mean
                           (- population-mean-one population-mean-two))
                        (/ standard-deviation
                           (Math/sqrt size)))
        :dof (- size 1)))))


(defrecord OneTail [dof alpha]
  Test

  (significance [type]
    (assoc type :critical-value (mget t-table (dec dof)
                                      ({0.05 0 0.025 1 0.01 2 0.005 3 0.0025 4 0.001 5 0.0005 6} alpha)))))


(defrecord TwoTail [dof alpha]
  Test

  (significance [type]
    (assoc type :critical-value (mget t-table (dec dof)
                                      ({0.1 0 0.05 1 0.02 2 0.01 3 0.005 4 0.002 5 0.001 6} alpha)))))



