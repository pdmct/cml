(ns rclojure.core.rfn
  (:refer-clojure :exclude [remove cat]))


(defprotocol ^{:private true} rcoll-type
  (string-vec [s] "Vector or strings")
  (int-vec [s] "Vector of ints"))


(defrecord ^{:private true} c [coll]
  rcoll-type
  (string-vec [this] (str "c(\"" (reduce str (interpose "\",\"" coll)) "\")"))
  (int-vec [this] (str "c(" (reduce str (interpose "," coll)) ")")))


(defn remove
  ([binding]
   (str "remove("binding")"))
  ([binding binding1]
    (str "remove("binding","binding1")")))


(defn sum
  ([coll]
   (str "sum("coll", rm.na = FALSE)"))
  ([coll rm-na?]
    (if (:rm-na? rm-na?)
       (str "sum(" coll ", rm.na = TRUE)")
       (str "sum(" coll ", rm.na = FALSE)"))))


(defn abs ([coll] (str "abs("coll")")))


(defn append
  ([coll coll1]
   (str "append("coll","coll1")"))
  ([coll coll1 {:keys [after]}]
   (if after
     (str "append("coll","coll1", after = "after")")
     (str "append("coll","coll1")"))))


(defn cat
  ([coll] (str "cat("coll")"))
  ([coll {:keys [file sep fill labels]}]
   (cond (and file sep (instance? Boolean fill) labels)
         (str "cat("coll", file = \""file"\", sep = \""sep"\", fill = "(.toUpperCase (str fill))", labels = " (if (nil? labels) "NULL" (string-vec (->c labels)))")")
         (and file sep (instance? Boolean fill))
         (str "cat("coll", file = \""file"\", sep = \""sep"\", fill = "(.toUpperCase (str fill))")")
         (and file sep)
         (str "cat("coll", file = \""file"\", sep = \""sep"\")")
         file
         (str "cat("coll", file = \""file"\")"))))


(defn matrix
  ([coll {:keys [nrow ncol byrow rows cols]}]
   (cond (and coll nrow ncol (instance? Boolean byrow) rows cols)
         (str "matrix("coll", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))", dimnames = list("(string-vec (->c rows))","(string-vec (->c cols))"))")
         (and coll nrow ncol (instance? Boolean byrow) rows)
         (str "matrix("coll", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))", dimnames = list("(string-vec (->c rows))"))")
         (and coll nrow ncol (instance? Boolean byrow))
         (str "matrix("coll", nrow = "nrow", ncol = "ncol", byrow = "(.toUpperCase (str byrow))")")
         (and coll nrow ncol)
         (str "matrix("coll", nrow = "nrow", ncol = "ncol")")
         (and coll nrow)
         (str "matrix("coll", nrow = "nrow")")
         coll
         (str "matrix("coll")"))))


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


