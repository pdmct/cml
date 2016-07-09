(ns cml.core.utils.statistics)

(defn mean [dataset] (double (/ (reduce + dataset) (count dataset))))

(defn mean-1 [x] (double (/ (reduce + x) (dec (count x)))))

(defn difference [{:keys [s1 s2]}] (map - s1 s2))

(defmulti standard-deviation (fn [type] (:type type)))

(defmethod standard-deviation :population [type]
  (Math/sqrt (mean (map (fn [x] (* (- (:mean type) x) (- (:mean type) x)))
                        (:data type)))))


(defmethod standard-deviation :sample [type]
  (Math/sqrt (mean-1 (map (fn [x] (* (- (:mean type) x) (- (:mean type) x))) (:data type)))))


(defmulti variance (fn [type] (:type type)))

(defmethod variance :population [type]
  (let [m (mean (:data type))]
    (/ (reduce + (map (fn [x] (* (- x m) (- x m)))
                    (:data type)))
       (count (:data type)))))


(defmethod variance :sample [type]
  (let [m (mean (:data type))]
    (/ (reduce + (map (fn [x] (* (- x m) (- x m)))
                    (:data type)))
       (dec (count (:data type))))))


(defmethod variance :pooled [type]
  (let [c (- (count (:data type)) 1)]
    (/ (* c (variance {:data (:data type) :type :sample})) c)))


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


