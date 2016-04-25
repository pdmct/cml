(ns cml.core.inference.hypothesis.ttest)


(defn- one-sample [mean standard-deviation hypo-mean size dof]
  {:mean               mean
   :standard-deviation standard-deviation
   :hypo-mean          hypo-mean
   :size               size
   :dof                dof
   :type               :one-sample})

(defn- two-sample [s1-mean s2-mean s1-pooled-variance s2-pooled-variance s1-size s2-size dof]
  {:s1-mean            s1-mean
   :s2-mean            s2-mean
   :s1-pooled-variance s1-pooled-variance
   :s2-pooled-variance s2-pooled-variance
   :s1-size            s1-size
   :s2-size            s2-size
   :dof                dof
   :type               :two-sample})


(defn one-sample-t-test [{:keys [mean standard-deviation hypo-mean size]}]
  ((comp (fn [x]
           (assoc x :t-statistic
                    (/ (- (:mean x) (:hypo-mean x))
                       (/ (:standard-deviation x) (Math/sqrt (:size x)))))))
    (one-sample mean
                standard-deviation
                hypo-mean
                size
                (dec size))))


(defmulti two-sample-t-test (fn [x _ _] (:two-sample-t-test x)))


(defmethod two-sample-t-test :equal-variance [exec {:keys [s1-mean s1-pooled-variance s1-size]} {:keys [s2-mean s2-pooled-variance s2-size]}]
  ((comp (fn [m]
           (assoc m :t-statistic (/ (- (:s1-mean m) (:s2-mean m))
                                    (Math/sqrt (* (/ (+ (:s1-pooled-variance m) (:s2-pooled-variance m)) 2)
                                                  (+ (/ 1 (:s1-size m))
                                                     (/ 1 (:s2-size m)))))) :exec exec)))
    (two-sample s1-mean s2-mean s1-pooled-variance s2-pooled-variance s1-size s2-size (- (+ s1-size s2-size) 2))))


(defmethod two-sample-t-test :unequal-variance [exec {:keys [s1-mean s1-pooled-variance s1-size]} {:keys [s2-mean s2-pooled-variance s2-size]}]
  ((comp (fn [m]
           (assoc m :t-statistic (/ (- (:s1-mean m) (:s2-mean m))
                                    (Math/sqrt (+
                                                 (/ (:s1-pooled-variance m)
                                                    (:s1-size m))
                                                 (/ (:s2-pooled-variance m)
                                                    (:s2-size m))))) :exec exec)))
    (two-sample s1-mean s2-mean s1-pooled-variance s2-pooled-variance s1-size s2-size (- (+ s1-size s2-size) 2))))


