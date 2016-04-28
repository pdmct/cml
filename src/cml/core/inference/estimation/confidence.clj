(ns cml.core.inference.estimation.confidence)


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn- one-sample [mean standard-deviation size critical-val]
  {:mean               mean
   :standard-deviation standard-deviation
   :size               size
   :critical-val       critical-val
   :type               :one-sample})


(defn- two-sample [mean variance size critical-val]
  {:mean         mean
   :variance     variance
   :size         size
   :critical-val critical-val
   :type         :two-sample})


(defn one-sample-confidence-interval [{:keys [mean standard-deviation size critical-val]}]
  ((comp (fn [one-sample]
           (assoc one-sample :upper (+ mean (* (:critical-val one-sample)
                                               (/ (:standard-deviation one-sample)
                                                  (Math/sqrt (:size one-sample)))))
                             :lower (- mean (* (:critical-val one-sample)
                                               (/ (:standard-deviation one-sample) (Math/sqrt (:size one-sample))))))))
    (one-sample mean standard-deviation size critical-val)))


(defn two-sample-confidence-interval [{:keys [mean variance size critical-val]}]
  ((comp (fn [x]
           (assoc x :upper (+ (- ((:mean x) 0) ((:mean x) 1))
                              (* (:critical-val x)
                                 (Math/sqrt (+
                                              (/ ((:variance x) 0) ((:size x) 0))
                                              (/ ((:variance x) 1) ((:size x) 1))))))
                    :lower (- (- ((:mean x) 0) ((:mean x) 1))
                              (* (:critical-val x)
                                 (Math/sqrt (+
                                              (/ ((:variance x) 0) ((:size x) 0))
                                              (/ ((:variance x) 1) ((:size x) 1)))))))))
    (two-sample mean variance size critical-val)))


