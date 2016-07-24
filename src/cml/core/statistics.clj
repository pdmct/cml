(ns cml.core.utils.statistics)

;TODO START DOCUMENTING!!

(defn mean [data] (double (/ (reduce + data) (count data))))

(defn mean-1 [data] (double (/ (reduce + data) (dec (count data)))))

(defn difference [{:keys [sample-one sample-two]}] (map - sample-one sample-two))


(defmulti variation :StandardDeviation)

(defn population [mean data] {:mean mean :data data :StandardDeviation :Population})

(defmethod variation :Population [data]
  (Math/sqrt (mean (map (fn [x] (* (- (:mean data) x) (- (:mean data) x))) (:data data)))))


(defn sample [mean data] {:mean mean :data data :StandardDeviation :Sample})

(defmethod variation :Sample [data]
  (Math/sqrt (mean-1 (map (fn [x] (* (- (:mean data) x) (- (:mean data) x))) (:data data)))))


(defmulti variance (fn [type] (:type type)))

(defmethod variance :population [type]
  (/ (reduce + (map (fn [x] (* (- x (:mean type)) (- x (:mean type))))
                    (:data type)))
     (count (:data type))))


(defmethod variance :sample [type]
  (/ (reduce + (map (fn [x] (* (- x (:mean type)) (- x (:mean type))))
                    (:data type)))
     (dec (count (:data type)))))


(defmethod variance :pooled [type]
  (/ (* (:size-1 type)
        (variance {:data (:data type) :mean (:mean type) :type :sample}))
     (:size-1 type)))


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


