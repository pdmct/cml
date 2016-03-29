(ns rclojure.core.rfn
  (:refer-clojure :exclude [remove cat]))


(defn vec-interp
  [coll]
  (cond
    (some string? coll)
    (str "c(\""(reduce str (interpose "\",\"" coll))"\")")
    (some #(instance? Double %) coll)
    (str "as.vector(c("(reduce str (interpose "," (double-array coll)))"), mode = \"double\")")
    :else (str "as.vector(c("(reduce str (interpose "," coll))"), mode = \"integer\")")))


(defn mtrx-interp
  [coll]
  (cond
    (> (count (drop-while #(not (some string? %)) coll)) 0)
    (map #(str "c(\""(reduce str (interpose "\",\"" %))"\")") coll)))


(defn remove
  ([sym]
   (str "remove("sym")"))
  ([sym sym1]
    (str "remove("sym","sym1")")))


(defn sum
  ([coll]  (str "sum(" (vec-interp coll) ", na.rm = FALSE)"))
  ([coll {:keys [na-rm?]}]
    (cond
      (and coll (instance? Boolean na-rm?))
      (str "sum(" (vec-interp coll) ", na.rm = " (.toUpperCase (str na-rm?)) ")")
      coll
      (str "sum(" (vec-interp coll) ")"))))


(defn abs ([coll] (str "abs("(vec-interp coll)")")))


(defn append
  ([coll coll1] (str "append(" (vec-interp coll)"," (vec-interp coll1) ")"))
  ([coll coll1 {:keys [after]}]
   (cond
     (and coll coll1 after)
     (str "append(" (vec-interp coll)"," (vec-interp coll1) ", after = "  after ")")
     (and coll coll1)
     (str "append(" (vec-interp coll) "," (vec-interp coll1) ")"))))


(defn cat
  ([coll] (str "cat(" (vec-interp coll)")"))
  ([coll {:keys [file sep fill labels]}]
   (cond (and coll file sep (instance? Boolean fill) labels)
         (str "cat(" (vec-interp coll)", file = \""file"\", sep = \""sep"\", fill = "(.toUpperCase (str fill))", labels = " (if (nil? labels) "NULL" (vec-interp labels))")")
         (and coll file sep (instance? Boolean fill))
         (str "cat(" (vec-interp coll)", file = \""file"\", sep = \""sep"\", fill = "(.toUpperCase (str fill))")")
         (and coll file sep)
         (str "cat(" (vec-interp coll)", file = \""file"\", sep = \""sep"\")")
         (and coll file)
         (str "cat(" (vec-interp coll)", file = \""file"\")")
         coll
         (str "cat(" (vec-interp coll) ")"))))


(defn matrix
  ([coll] (str "matrix("coll")"))
  ([coll {:keys [nrow ncol byrow rows cols]}]
   (cond (and coll nrow ncol (instance? Boolean byrow) rows cols)
         (str "matrix(" (vec-interp coll) ", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))", dimnames = list("(vec-interp rows)","(vec-interp cols)"))")
         (and coll nrow ncol (instance? Boolean byrow) rows)
         (str "matrix(" (vec-interp coll) ", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))", dimnames = list("(vec-interp rows)"))")
         (and coll nrow ncol (instance? Boolean byrow))
         (str "matrix(" (vec-interp coll) ", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))")")
         (and coll nrow ncol)
         (str "matrix(" (vec-interp coll) ", nrow = "nrow", ncol = "ncol")")
         (and coll nrow)
         (str "matrix(" (vec-interp coll) ", nrow = "nrow")")
         coll
         (str "matrix(" (vec-interp coll) ")"))))


(defn plot [coll] (str "plot("coll")"))


(defn plot-vec ([coll] (str "plot(" (vec-interp coll) ")")))


(defn dev-off [] (str "dev.off()"))


(defn jpg
  ([{:keys [fname]}]
   (if
     (or (.endsWith fname ".jpg") (.endsWith  fname ".JPG"))
     (str "jpeg(\""fname"\")")
     (throw (IllegalArgumentException. (str fname "must have the prefix .jpg or .JPG"))))))


(defn pdf
  ([{:keys [fname]}]
   (if
     (or (.endsWith fname ".pdf") (.endsWith  fname ".PDF"))
     (str "pdf(\""fname"\")")
     (throw (IllegalArgumentException. (str fname "must have the prefix .pdf or .PDF"))))))



