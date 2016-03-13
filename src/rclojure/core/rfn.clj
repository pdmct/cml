(ns rclojure.core.rfn
  (:refer-clojure :exclude [remove cat]))


(defn c
  ([& vals] (str "c(" (reduce str (interpose "," vals)) ")")))


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
      (do (println "TRUE") (str "sum(" coll ", rm.na = TRUE)"))
      (do (println "FALSE") (str "sum(" coll ", rm.na = FALSE)")))))


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
  ([coll {:keys [file sep fill? labels]}]
   (cond (and file sep (not= fill? nil) labels)
         (str "cat("coll", file = "file", sep = "sep", fill = "(.toUpperCase (str fill?))", "labels")")
         (and file sep (not= fill? nil))
         (str "cat("coll", file = "file", sep = "sep", fill = "fill?")")
         (and file sep)
         (str "cat("coll", file = "file", sep = "sep")")
         file
         (str "cat("coll", file = " "\"file"\"")"))))