(ns cml.core.inference.estimate.confidence-interval)

;TODO Have functions comply with dataframes

;TODO have in out map

(defprotocol Estimate
  (one-sample [os] "One sample confidence interval estimation")
  (two-sample [ts] "Two sample confidence interval estimation"))

;TODO change to multimethods

(defn one-sample-confidence-interval [mean standard-deviation size critical-value]
  {:mean mean
   :standard-deviation standard-deviation
   :size size
   :critical-value critical-value
   :Estimate :OneSample})


(defn two-sample-confidence-interval [mean variance size critical-value]
  {:mean mean
   :variance variance
   :size size
   :critical-value critical-value
   :Estimate :TwoSample})


(defrecord ConfidenceInterval [confidence-interval]
  Estimate

  (one-sample [type]
    (assoc type
      :upper (+ (:mean confidence-interval)
                (* (:critical-value confidence-interval)
                   (/ (:standard-deviation confidence-interval)
                      (Math/sqrt (:size confidence-interval)))))
      :lower (- (:mean confidence-interval)
                (* (:critical-value confidence-interval)
                   (/ (:standard-deviation confidence-interval)
                      (Math/sqrt (:size confidence-interval)))))))

  (two-sample [type]
    (assoc type
      :upper (+ (- ((:mean confidence-interval) 0)
                   ((:mean confidence-interval) 1))
                (* (:critical-value confidence-interval)
                   (Math/sqrt (+ (/ ((:variance confidence-interval) 0)
                                    ((:size confidence-interval) 0))
                                 (/ ((:variance confidence-interval) 1)
                                    ((:size confidence-interval) 1))))))
      :lower (- (- ((:mean confidence-interval) 0)
                   ((:mean confidence-interval) 1))
                (* (:critical-value confidence-interval)
                   (Math/sqrt (+ (/ ((:variance confidence-interval) 0)
                                    ((:size confidence-interval) 0))
                                 (/ ((:variance confidence-interval) 1)
                                    ((:size confidence-interval) 1)))))))))


