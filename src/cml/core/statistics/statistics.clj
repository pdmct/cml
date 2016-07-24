(ns cml.core.statistics.statistics)

;TODO START DOCUMENTING!!

(defn mean [data] (double (/ (reduce + data) (count data))))

(defn mean-1 [data] (double (/ (reduce + data) (dec (count data)))))

(defn difference [{:keys [sample-one sample-two]}] (map - sample-one sample-two))


(defmulti standard-deviation :Variation)

(defmethod standard-deviation :Sample [{:keys [mean data Variation]}]
  {:mean mean :data data :Variation Variation})

(defmethod standard-deviation :Population [{:keys [mean data Variation]}]
  {:mean mean :data data :Variation Variation})

(defmethod standard-deviation :default [x] {:err x})


(defprotocol Variation
  (population-variation [pv] "Population variation")
  (sample-variation [sv] "Sample variation")
  (pooled-variation [pv] "Pooled variation"))

(defrecord StandardDeviation [standard-deviation]
  Variation

  (population-variation [type]
    (assoc type
      :population-standard-deviation
      (Math/sqrt (mean (map #(* (- (:mean standard-deviation) %) (- (:mean standard-deviation) %))
                            (:data standard-deviation))))))

  (sample-variation [type]
    (assoc type
      :sample-standard-deviation
      (Math/sqrt (mean-1 (map #(* (- (:mean standard-deviation) %) (- (:mean standard-deviation) %))
                              (:data standard-deviation)))))))



(defmulti variance :Variation)

(defmethod variance :Sample [{:keys [mean data Variation]}]
  {:mean mean :data data :Variation Variation})

(defmethod variance :Population [{:keys [mean data Variation]}]
  {:mean mean :data data :Variation Variation})

(defmethod variance :Pooled [{:keys [mean data size-1 Variation]}]
  {:mean mean :size-1 size-1 :data data :Variation Variation})

(defmethod variance :default [x] {:err x})


(defrecord Variance [variance]
  Variation

  (population-variation [type]
    (assoc type :population-variance
      (/ (reduce + (map #(* (- % (:mean variance)) (- % (:mean variance))) (:data variance)))
         (count (:data variance)))))

  (sample-variation [type]
    (assoc type :sample-variance
      (/ (reduce + (map #(* (- % (:mean variance)) (- % (:mean variance))) (:data variance)))
         (dec (count (:data variance))))))

  (pooled-variation [type]
    (assoc type :pooled-variance
                (/ (* (:size-1 variance)
                      (/ (reduce + (map #(* (- % (:mean variance)) (- % (:mean variance))) (:data variance)))
                         (dec (count (:data variance)))))
                   (:size-1 variance)))))


(defn permutations
  [x xs]
  (letfn [(factorial [x]
            (loop [cnt (if (coll? x)
                         (count x) x) acc 1]
              (if (zero? cnt)
                acc (recur (dec cnt) (*' cnt acc)))))]
    (quot (factorial x)
          (factorial (- x xs)))))


(defn significance [correlation sample-size]
  (/ (* correlation
        (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


