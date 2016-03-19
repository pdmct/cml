(ns rclojure.core.rfn
  (:refer-clojure :exclude [remove cat]))


(defprotocol rvec-type
  (string [s] "Vector or strings")
  (integer [s] "Vector of ints"))


(defrecord ^{:private true} c [coll]
  rvec-type
  (string [this] (str "c(\"" (reduce str (interpose "\",\"" coll)) "\")"))
  (integer [this] (str "c(" (reduce str (interpose "," coll)) ")")))


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
         (str "cat("coll", file = \""file"\", sep = \""sep"\", fill = "(.toUpperCase (str fill))", labels = " (if (nil? labels) "NULL" (string (->c labels)))")")
         (and file sep (instance? Boolean fill))
         (str "cat("coll", file = \""file"\", sep = \""sep"\", fill = "(.toUpperCase (str fill))")")
         (and file sep)
         (str "cat("coll", file = \""file"\", sep = \""sep"\")")
         file
         (str "cat("coll", file = \""file"\")"))))


(defn plot ([coll] (str"plot("coll")")))


