(ns cml.core.statistics.statistics)

;TODO START DOCUMENTING!!

(defn mean [data] (double (/ (reduce + data) (count data))))

(defn mean-1 [data] (double (/ (reduce + data) (dec (count data)))))

(defn difference [{:keys [sample-one sample-two]}] (map - sample-one sample-two))


(defprotocol Variation
  (standard-deviation [sd] "Standard deviation")
  (variance [v] "Variance"))


(defrecord Sample [sample-mean sample]
  Variation

  (standard-deviation [type]
    (assoc type
      :standard-deviation
      (Math/sqrt (mean-1 (map #(* (- sample-mean %) (- sample-mean %))
                              sample)))))

  (variance [type]
    (assoc type
      :variance
      (/ (reduce + (map #(* (- % sample-mean) (- % sample-mean))
                        sample))
         (dec (count sample))))))


(defrecord Population [population-mean population]
  Variation

  (standard-deviation [type]
    (assoc type
      :standard-deviation
      (Math/sqrt (mean (map #(* (- population-mean %)
                                (- population-mean %)) population)))))

  (variance [type]
    (assoc type :variance
                (/ (reduce + (map #(* (- % population-mean) (- % population-mean))
                                  population))
                   (count population)))))


(defrecord Pooled [pooled-mean pooled-data size-1]
  Variation

  (variance [type]
    (assoc type :variance
                (/ (* size-1 (/ (reduce + (map #(* (- % pooled-mean) (- % pooled-mean))
                                               pooled-data))
                                (dec (count pooled-data))))
                   size-1))))


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


