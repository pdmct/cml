(ns cml.core.inference.estimate.confidence-interval)

;TODO Have functions comply with dataframes

(defprotocol Estimate
  (confidence-interval [ci] "Confidence imterval"))


(defrecord OneSample [sample-mean sample-standard-deviation sample-size critical-value]
  Estimate

  (confidence-interval [type]
    (assoc type
      :upper (+ sample-mean
                (* critical-value
                   (/ sample-standard-deviation
                      (Math/sqrt sample-size))))
      :lower (- sample-mean
                (* critical-value
                   (/ sample-standard-deviation
                      (Math/sqrt sample-size)))))))


(defrecord TwoSample [sample-mean sample-variance sample-size critical-value]
  Estimate

  (confidence-interval [type]
    (assoc type
      :upper (+ (- (first sample-mean)
                   (second sample-mean))
                (* critical-value
                   (Math/sqrt (+ (/ (first sample-variance)
                                    (first sample-size))
                                 (/ (second sample-variance)
                                    (second sample-size))))))
      :lower (- (- (first sample-mean)
                   (second sample-mean))
                (* critical-value
                   (Math/sqrt (+ (/ (first sample-variance)
                                    (first sample-size))
                                 (/ (second sample-variance)
                                    (second sample-size)))))))))


