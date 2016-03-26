(ns rclojure.core.rfn
  (:refer-clojure :exclude [remove cat]))


(defn c
  [coll]
  (cond
    (some string? coll)
    (str "c(\""(reduce str (interpose "\",\"" coll))"\")")
    (some #(instance? Double %) coll)
    (str "c("(reduce str (interpose "," (double-array coll)))")")
    :else (str "c("(reduce str (interpose "," coll))")")))


(defn remove
  ([sym]
   (str "remove("sym")"))
  ([sym sym1]
    (str "remove("sym","sym1")")))


(defn sum
  ([coll]  (str "sum(" (c coll) ", na.rm = FALSE)"))
  ([coll {:keys [na-rm?]}]
    (cond
      (and coll (instance? Boolean na-rm?))
      (str "sum(" (c coll) ", na.rm = " (.toUpperCase (str na-rm?)) ")")
      coll
      (str "sum(" (c coll) ")"))))


(defn abs ([coll] (str "abs("(c coll)")")))


(defn append
  ([coll coll1] (str "append(" (c coll)"," (c coll1) ")"))
  ([coll coll1 {:keys [after]}]
   (cond
     (and coll coll1 after)
     (str "append(" (c coll)"," (c coll1) ", after = "  after ")")
     (and coll coll1)
     (str "append(" (c coll) "," (c coll1) ")"))))


(defn cat
  ([coll] (str "cat(" (c coll)")"))
  ([coll {:keys [file sep fill labels]}]
   (cond (and coll file sep (instance? Boolean fill) labels)
         (str "cat(" (c coll)", file = \""file"\", sep = \""sep"\", fill = "(.toUpperCase (str fill))", labels = " (if (nil? labels) "NULL" (c labels))")")
         (and coll file sep (instance? Boolean fill))
         (str "cat(" (c coll)", file = \""file"\", sep = \""sep"\", fill = "(.toUpperCase (str fill))")")
         (and coll file sep)
         (str "cat(" (c coll)", file = \""file"\", sep = \""sep"\")")
         (and coll file)
         (str "cat(" (c coll)", file = \""file"\")")
         coll
         (str "cat(" (c coll) ")"))))


(defn matrix
  ([coll] (str "matrix("coll")"))
  ([coll {:keys [nrow ncol byrow rows cols]}]
   (cond (and coll nrow ncol (instance? Boolean byrow) rows cols)
         (str "matrix(" (c coll) ", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))", dimnames = list("(c rows)","(c cols)"))")
         (and coll nrow ncol (instance? Boolean byrow) rows)
         (str "matrix(" (c coll) ", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))", dimnames = list("(c rows)"))")
         (and coll nrow ncol (instance? Boolean byrow))
         (str "matrix(" (c coll) ", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))")")
         (and coll nrow ncol)
         (str "matrix(" (c coll) ", nrow = "nrow", ncol = "ncol")")
         (and coll nrow)
         (str "matrix(" (c coll) ", nrow = "nrow")")
         coll
         (str "matrix(" (c coll) ")"))))


(defn plot ([coll] (str "plot("coll")")))


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


