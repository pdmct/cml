(ns cml.core.inference.t-test
  (:require [cml.core.inference.tables :refer [t-table]]))
(use 'clojure.core.matrix)

;TODO Have functions comply with dataframes


(defprotocol Test
  (one-sample [one] "One tail t-test")
  (equal-variance [two] "Two tail t-test")
  (welch [welch] "Welsch's t-test")
  (repeated-measure [rm] "Repeated measure t-test"))

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

(defrecord TTest [type]
  Test
  (one-sample [this]
    (assoc type
      :t-statistic (/ (- (:mean type)
                         (:hypothetical-mean type))
                      (/ (:standard-deviation type)
                         (Math/sqrt (:size type))))

      :dof (dec (:size type))))

  (equal-variance [this]
    (assoc type
      :t-statistic (/ (- (- ((:sample-mean type) 0)
                            ((:sample-mean type) 1))
                         (- ((:population-mean type) 0)
                            ((:population-mean type) 1)))
                      (Math/sqrt (* (/ (+ ((:pooled-variance type) 0)
                                          ((:pooled-variance type) 1))
                                       2)
                                    (+ (/ 1
                                          ((:size type) 0))
                                       (/ 1
                                          ((:size type) 1))))))
      :dof (- (+ ((:size type) 0)
                 ((:size type) 1)) 2)))

  (welch [this]
    (assoc type
      :t-statistic (/ (- ((:mean type) 0)
                         ((:mean type) 1))
                      (Math/sqrt (+ (/ ((:sample-variance type) 0)
                                       ((:size type) 0))
                                    (/ ((:sample-variance type) 1)
                                       ((:size type) 1)))))
      :dof (/ (* (+ (/ ((:sample-variance type) 0)
                       ((:size type) 0))
                    (/ ((:sample-variance type) 1)
                       ((:size type) 1)))
                 (+ (/ ((:sample-variance type) 0)
                       ((:size type) 0))
                    (/ ((:sample-variance type) 1)
                       ((:size type) 1))))
              (+ (/ (* (/ ((:sample-variance type) 0)
                          ((:size type) 0))
                       (/ ((:sample-variance type) 0)
                          ((:size type) 0)))
                    (- ((:size type) 0)
                       1))
                 (/ (* (/ ((:sample-variance type) 1)
                          ((:size type) 1))
                       (/ ((:sample-variance type) 1)
                          ((:size type) 1)))
                    (- ((:size type) 1)
                       1))))))

  (repeated-measure [this]
    (assoc type
      :t-statistic (/ (- (:difference-mean type)
                         (- ((:population-mean type) 0)
                            ((:population-mean type) 1)))
                      (/ (:standard-deviation type)
                         (Math/sqrt (:size type))))
      :dof (- (:size type) 1))))


(defmulti critical-value :SignificanceTest)

(defn one-tail-test [dof alpha]
  {:dof dof
   :alpha alpha
   :SignificanceTest :OneTail})

(defn two-tail-test [dof alpha]
  {:dof dof
   :alpha alpha
   :SignificanceTest :TwoTail})

(defmethod critical-value :OneTail [type]
  (assoc type
    :critical-value (mget t-table (dec (:dof type))
                          ({0.05 0 0.025 1 0.01 2 0.005 3 0.0025 4 0.001 5 0.0005 6}
                            (:alpha type)))))

(critical-value (one-tail-test 9 0.05))

(defmethod critical-value :TwoTail [type]
  (assoc type
    :critical-value (mget t-table (dec (:dof type))
                          ({0.1 0 0.05 1 0.02 2 0.01 3 0.005 4 0.002 5 0.001 6}
                            (:alpha type)))))

(critical-value (two-tail-test 9 0.05))


(defn one-tail [type]
  (assoc type
    :critical-value (mget t-table
                          (dec (:dof type))
                          ({0.05 0 0.025 1 0.01 2 0.005 3 0.0025 4 0.001 5 0.0005 6}
                            (:alpha type)))))



(defn two-tail [type]
  (assoc type
    :critical-value (mget t-table
                          (dec (:dof type))
                          ({0.1 0 0.05 1 0.02 2 0.01 3 0.005 4 0.002 5 0.001 6}
                            (:alpha type)))))


(def test-hierarchy (-> (make-hierarchy)
                        (derive :TTest :Test)
                        (derive :SignificanceTest :Test)))





