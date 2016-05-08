(ns cml.core.utils.stats)


(defn mean [x] (double (/ (reduce + x) (count x))))

(defn mean-1 [x] (double (/ (reduce + x) (dec (count x)))))

(defn coefficient-determination [rho] (* rho rho))

(defmulti standard-deviation (fn [x _] (:standard-deviation x)))

(defmethod standard-deviation :population [x population]
  (let [m (mean population)]
    (assoc x :val
             (Math/sqrt (mean (map (fn [x] (* (- m x) (- m x))) population))))))


(defmethod standard-deviation :sample [x sample]
  (let [m (mean sample)]
    (assoc x :val
             (Math/sqrt (mean-1 (map (fn [x] (* (- m x) (- m x))) sample))))))


(defmulti variance (fn [x _] (:variance x)))

(defmethod variance :population [x population]
  (assoc x :val
           (let [m (mean population)]
             (/ (reduce + (map (fn [x] (* (- x m) (- x m))) population)) (count population)))))


(defmethod variance :sample [x sample]
  (let [m (mean sample)]
    (assoc x :val
             (/ (reduce + (map (fn [x] (* (- x m) (- x m))) sample))
                (dec (count sample))))))


(defmethod variance :pooled [x sample]
  (let [c (- (count sample) 1)]
    (assoc x :val
             (/ (* c (:val (variance {:variance :sample} sample))) c))))


(defn permutations
  [x xs]
  (try
    (letfn [(factorial [x]
              (loop [cnt (if (coll? x) (count x) x) acc 1]
                (if (zero? cnt) acc
                                (recur (dec cnt) (*' cnt acc)))))]
      (quot (factorial x) (factorial (- x xs))))
    (catch Exception e nil)))


(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


