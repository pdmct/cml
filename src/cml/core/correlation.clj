(ns cml.core.correlation)

(def sample {:x [1 4 7 3 55 87 4 3 66 5 4 3 2 66] :y [6 5 44 2 4 88 7 5 2 1 3 4 6 3]})


;Independent variable goes on the x-axis
;Dependant on the y axis
;The steps to calculate the sum of squares for x are as follows:

(defn pearson [{:keys [independant dependant]}])

;1) For each x score, subtract the mean of x as calculated from the sample. This is called the deviation score.

(def mean (/ (reduce + (:x sample)) (count (:x sample))))

(map #(- mean %) (:x sample))


