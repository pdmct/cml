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
  ((comp (fn [x]
           (assoc x :t-statistic
                    (/ (- (:mean x) (:hypo-mean x))
                       (/ (:standard-deviation x) (Math/sqrt (:size x)))))))
    (one-sample mean
                standard-deviation
                hypo-mean
                size
                (dec size))))


(defmulti two-sample-t-test (fn [x _] (:two-sample-t-test x)))


(defmethod two-sample-t-test :equal-variance [exec {:keys [mean pooled-variance size]}]
  ((comp (fn [m]
           (assoc m :t-statistic (/ (- (first (:mean m)) (second (:mean m)))
                                    (Math/sqrt (* (/ (+ (first (:pooled-variance m)) (second (:pooled-variance m))) 2)
                                                  (+ (/ 1 (first (:size m)))
                                                     (/ 1 (second (:size m))))))) :exec exec)))
    (two-sample mean pooled-variance size (- (+ (first size) (second size)) 2))))


(defmethod two-sample-t-test :unequal-variance [exec {:keys [mean pooled-variance size]}]
  ((comp (fn [m]
           (assoc m :t-statistic (/ (- (first (:mean m)) (second (:mean m)))
                                    (Math/sqrt (+
                                                 (/ (first (:pooled-variance m)) (first (:size m)))
                                                 (/ (second (:pooled-variance m)) (second (:size m)))))) :exec exec)))
    (two-sample mean pooled-variance size (- (+ (first size) (second size)) 2)))) ;TODO remove this step and call to first & second


