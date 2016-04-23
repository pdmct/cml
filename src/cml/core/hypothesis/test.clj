(ns cml.core.hypothesis.test)


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn- one-sample [mean sd hypo-mean size]
  {:mean      mean
   :hypo-mean hypo-mean
   :sd        sd
   :size      size})


(defn one-sample-t-test [{:keys [mean sd hypo-mean size]}]
  ((comp (fn [x]
           (assoc x :t-statistic
                    (/ (- (:mean x) (:hypo-mean x))
                       (/ (:sd x) (Math/sqrt (:size x)))))))
    (one-sample mean
                sd
                hypo-mean
                size)))


(defn- two-sample [s1-mean s2-mean s1-pool-var s2-pool-var s1-size s2-size]
  {:s1-mean     s1-mean
   :s2-mean     s2-mean
   :s1-pool-var s1-pool-var
   :s2-pool-var s2-pool-var
   :s1-size     s1-size
   :s2-size     s2-size})


(defmulti two-sample-t-test (fn [x _ _] (:two-sample-t-test x)))

(defmethod two-sample-t-test :equal-variance [exec {:keys [s1-mean s1-pool-var s1-size]} {:keys [s2-mean s2-pool-var s2-size]}]
  ((comp (fn [m]
           (assoc m :t (/ (- (:s1-mean m) (:s2-mean m))
                          (Math/sqrt (* (/ (+ (:s1-pool-var m) (:s2-pool-var m)) 2)
                                        (+ (/ 1 (:s1-size m))
                                           (/ 1 (:s2-size m)))))) :exec exec)))
    (two-sample s1-mean s2-mean s1-pool-var s2-pool-var s1-size s2-size)))


(defmethod two-sample-t-test :unequal-variance [exec {:keys [s1-mean s1-pool-var s1-size]} {:keys [s2-mean s2-pool-var s2-size]}]
  ((comp (fn [m]
           (assoc m :t (/ (- (:s1-mean m) (:s2-mean m))
                          (Math/sqrt
                            (+
                              (/ (:s1-pool-var m)
                                 (:s1-size m))
                              (/ (:s2-pool-var m)
                                 (:s2-size m))))) :exec exec)))
    (two-sample s1-mean s2-mean s1-pool-var s2-pool-var s1-size s2-size)))


(defn conf-inter [mean sd size critical-val]
  {:mean         mean
   :sd           sd
   :size         size
   :critical-val critical-val})


(defn one-sample-conf-inter [{:keys [mean sd size critical-val]}]
  ((comp (fn [x]
           (assoc x :plus (+ mean (* (:critical-val x)
                                     (/ (:sd x) (Math/sqrt (:size x)))))
                    :minus (- mean (* (:critical-val x) (/ (:sd x) (Math/sqrt (:size x))))))))
    (conf-inter mean sd size critical-val)))



