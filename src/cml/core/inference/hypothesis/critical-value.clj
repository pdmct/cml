(ns cml.core.inference.hypothesis.critical-value)


(defmulti t-test (fn [type] (:type type)))

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
    :dof (- (:size type)
            1)))


