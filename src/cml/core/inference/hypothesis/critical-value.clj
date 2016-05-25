(ns cml.core.inference.hypothesis.critical-value)


(defmulti t-test (fn [type] (:type type)))

(defmethod t-test :one-sample [type]
  (assoc type
    :t-statistic (/ (- (:mean type)
                       (:hypo-mean type))
                    (/ (:standard-deviation type)
                       (Math/sqrt (:size type))))
    :dof (dec (:size type))
    :alpha (:alpha type)))


(defmethod t-test :equal-variance [type]
  (assoc type
    :t-statistic (/ (- ((:mean type) 0)
                       ((:mean type) 1))
                    (Math/sqrt (* (/ (+ ((:pooled-variance type) 0)
                                        ((:pooled-variance type) 1))
                                     2)
                                  (+ (/ 1
                                        ((:size type) 0))
                                     (/ 1
                                        ((:size type) 1))))))
    :dof (- (+ ((:size type) 0)
               ((:size type) 1))
            2)
    :alpha (:alpha type)))


(defmethod t-test :unequal-variance [type]
  (assoc type
    :t-statistic (/ (- ((:mean type) 0)
                       ((:mean type) 1))
                    (Math/sqrt (+ (/ ((:pooled-variance type) 0)
                                     ((:size type) 0))
                                  (/ ((:pooled-variance type) 1)
                                     ((:size type) 1)))))
    :dof (- (+ ((:size type) 0)
               ((:size type) 1))
            2)
    :alpha (:alpha type)))


(defmethod t-test :repeated-measure [type]
  (assoc type
    :t-statistic (/ (- (:difference-mean type)
                       (- ((:mean type) 0)
                          ((:mean type) 1)))
                    (/ (:standard-deviation type)
                       (Math/sqrt (:size type))))
    :dof (- (:size type)
            1)
    :alpha (:alpha type)))


