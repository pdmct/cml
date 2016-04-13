(ns cml.core.utils.stats)


(defn mean [x] (double (/ (reduce + x) (count x))))

(defn sample-mean [x] (double (/ (reduce + x) (dec (count x)))))

(defn coefficient-determination [rho] (* rho rho))

(defmulti standard-deviation (fn [x population] (:type x)))


(defmethod standard-deviation :population [x population]
  (let [m (mean population)]
    (Math/sqrt (mean (map (fn [x] (* (- m x) (- m x))) population)))))


(defmethod standard-deviation :sample [x sample]
  (let [m (mean sample)]
    (Math/sqrt (sample-mean (map (fn [x] (* (- m x) (- m x))) sample)))))


(defn permutations
  [x xs]
  (try
    (letfn [(factorial [x]
              (loop [cnt (if (coll? x) (count x) x) acc 1]
                (if (zero? cnt) acc
                                (recur (dec cnt) (*' cnt acc)))))]
      (quot (factorial x) (factorial (- x xs))))
    (catch Exception e nil)))


