(ns cml.core.inference.hypothesis.t-test
  (:require [cml.core.inference.tables :refer [t-table]]))
(use 'clojure.core.matrix)

;TODO Have functions comply with dataframes
(defmulti t-test (fn [type] (:type type)))

(defmulti hypothesis :TTest)

(defn one-sample [])
(defmethod hypothesis :OneSample [])                        ;TODO finish abstraction refactor

(defmethod t-test :one-sample [type]
  (assoc type
    :t-statistic (/ (- (:mean type)
                       (:hypo-mean type))
                    (/ (:standard-deviation type)
                       (Math/sqrt (:size type))))
    :dof (dec (:size type))))


(defmethod t-test :equal-variance [type]
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
               ((:size type) 1))
            2)))


(defmethod t-test :welch [type]
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


(defmethod t-test :repeated-measure [type]
  (assoc type
    :t-statistic (/ (- (:difference-mean type)
                       (- ((:population-mean type) 0)
                          ((:population-mean type) 1)))
                    (/ (:standard-deviation type)
                       (Math/sqrt (:size type))))
    :dof (- (:size type) 1)))


(defmulti critical-value :SignificanceTest)

(defn one-tail-test [{:keys [dof alpha]}]                   ;change to one-tail
  {:SignificanceTest :OneTail
   :dof dof
   :alpha alpha})

(defn two-tail-test [{:keys [dof alpha]}]                   ;change to two-tail
  {:SignificanceTest :TwoTail
   :dof dof
   :alpha alpha})

(defmethod critical-value :OneTail [type]
  (mget t-table
        (dec (:dof type))
        ({0.05   0
          0.025  1
          0.01   2
          0.005  3
          0.0025 4
          0.001  5
          0.0005 6} (:alpha type))))

(defmethod critical-value :TwoTail [type]
  (mget t-table
        (dec (:dof type))
        ({0.1   0
          0.05  1
          0.02  2
          0.01  3
          0.005 4
          0.002 5
          0.001 6} (:alpha type))))

;(critical-value (one-tail-test {:SignificanceTest :OneTail :dof 9 :alpha 0.05}))
;(critical-value (two-tail-test {:SignificanceTest :OneTail :dof 9 :alpha 0.05}))



(defn one-tail [type]
  (assoc type
    :critical-value (mget t-table
                          (dec (:dof type))
                          ({0.05   0
                            0.025  1
                            0.01   2
                            0.005  3
                            0.0025 4
                            0.001  5
                            0.0005 6} (:alpha type)))))



(defn two-tail [type]
  (assoc type
    :critical-value (mget t-table
                          (dec (:dof type))
                          ({0.1   0
                            0.05  1
                            0.02  2
                            0.01  3
                            0.005 4
                            0.002 5
                            0.001 6} (:alpha type)))))

