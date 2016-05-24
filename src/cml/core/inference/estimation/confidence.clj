(ns cml.core.inference.estimation.confidence)


(defmulti confidence-interval (fn [type] (:type type)))

(defmethod confidence-interval :one-sample [type]
  (assoc type
    :upper (+ (:mean type)
              (* (:critical-val type)
                 (/ (:standard-deviation type)
                    (Math/sqrt (:size type)))))
    :lower (- (:mean type)
              (* (:critical-val type)
                 (/ (:standard-deviation type)
                    (Math/sqrt (:size type)))))))


(defmethod confidence-interval :two-sample [type]
  (assoc type
    :upper (+ (- ((:mean type) 0)
                 ((:mean type) 1))
              (* (:critical-val type)
                 (Math/sqrt (+ (/ ((:variance type) 0)
                                  ((:size type) 0))
                               (/ ((:variance type) 1)
                                  ((:size type) 1))))))
    :lower (- (- ((:mean type) 0)
                 ((:mean type) 1))
              (* (:critical-val type)
                 (Math/sqrt (+ (/ ((:variance type) 0)
                                  ((:size type) 0))
                               (/ ((:variance type) 1)
                                  ((:size type) 1))))))))


