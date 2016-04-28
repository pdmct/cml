(ns cml.core.inference.hypothesis.critical-value)



(defn- one-sample [mean standard-deviation hypo-mean size dof]
  {:mean               mean
   :standard-deviation standard-deviation
   :hypo-mean          hypo-mean
   :size               size
   :dof                dof
   :type               :one-sample})


(defn- two-sample [mean pooled-variance size dof]
  {:mean            mean
   :pooled-variance pooled-variance
   :size            size
   :dof             dof
   :type            :two-sample})


(defn one-sample-t-test [{:keys [mean standard-deviation hypo-mean size]}]
  ((comp (fn [one-sample]
           (assoc one-sample :t-statistic
                             (/ (- (:mean one-sample) (:hypo-mean one-sample))
                                (/ (:standard-deviation one-sample) (Math/sqrt (:size one-sample)))))))
    (one-sample mean
                standard-deviation
                hypo-mean
                size
                (dec size))))


(defmulti two-sample-t-test (fn [x _] (:two-sample-t-test x)))


(defmethod two-sample-t-test :equal-variance [exec {:keys [mean pooled-variance size]}]
  ((comp (fn [two-sample]
           (assoc two-sample :t-statistic (/ (- ((:mean two-sample) 0) ((:mean two-sample) 1))
                                             (Math/sqrt (*
                                                          (/ (+ ((:pooled-variance two-sample) 0) ((:pooled-variance two-sample) 1)) 2)
                                                          (+ (/ 1 ((:size two-sample) 0)) (/ 1 ((:size two-sample) 1)))))) :exec exec)))
    (two-sample mean pooled-variance size (- (+ (size 0) (size 1)) 2))))


(defmethod two-sample-t-test :unequal-variance [exec {:keys [mean pooled-variance size]}]
  ((comp (fn [two-sample]
           (assoc two-sample :t-statistic (/ (- ((:mean two-sample) 0) ((:mean two-sample) 1))
                                    (Math/sqrt (+
                                                 (/ ((:pooled-variance two-sample) 0) ((:size two-sample) 0))
                                                 (/ ((:pooled-variance two-sample) 1) ((:size two-sample) 1))))) :exec exec)))
    (two-sample mean pooled-variance size (- (+ (size 0) (size 1)) 2))))


