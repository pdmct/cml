(ns cml.core.accuracy)


(defmulti sample-t-test (fn [x sample] (:sample x)))

(defmethod sample-t-test :one [x {:keys [sample-size]}] (- sample-size 1))

(defmethod sample-t-test :two [x {:keys [sample-size]}] (- sample-size 2))

(defn slr-test-slope [sample] (- (count sample) 2))


