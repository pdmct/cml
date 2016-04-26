(ns cml.core.inference.estimation.confidence)


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn- one-sample [mean sd size critical-val]
  {:mean               mean
   :standard-deviation sd
   :size               size
   :critical-val       critical-val})


(defn- two-sample [s1-mean s2-mean s1-variance s2-variance s1-size s2-size critical-val]
  {:s1-mean      s1-mean
   :s2-mean      s2-mean
   :s1-variance  s1-variance
   :s2-variance  s2-variance
   :s1-size      s1-size
   :s2-size      s2-size
   :critical-val critical-val})


(defn one-sample-confidence-interval [{:keys [mean standard-deviation size critical-val]}]
  ((comp (fn [x]
           (assoc x :plus (+ mean (* (:critical-val x)
                                     (/ (:standard-deviation x) (Math/sqrt (:size x)))))
                    :minus (- mean (* (:critical-val x) (/ (:standard-deviation x) (Math/sqrt (:size x))))))))
    (one-sample mean standard-deviation size critical-val)))


(defn two-sample-confidence-interval [{:keys [s1-mean s1-variance s1-size]} {:keys [s2-mean s2-variance s2-size]} critical-val]
  ((comp (fn [x]
           (assoc x :plus (+ (- (:s1-mean x) (:s2-mean x))
                             (* (:critical-val x) (Math/sqrt (+ (/ (:s1-variance x) (:s1-size x)) (/ (:s2-variance x) (:s2-size x))))))
                    :minus (- (- (:s1-mean x) (:s2-mean x))
                              (* (:critical-val x) (Math/sqrt (+ (/ (:s1-variance x) (:s1-size x)) (/ (:s2-variance x) (:s2-size x)))))))))
    (two-sample s1-mean s2-mean s1-variance s2-variance s1-size s2-size critical-val)))


