(ns cml.core.file
  (:require [clojure.java.io :as io])
  (:import (java.io BufferedReader InputStreamReader FileInputStream)
           (java.util.zip GZIPInputStream)))


(defmulti read-file (fn [x _] (:file x)))


(defmethod read-file :csv
  [_ {:keys [path]}]
  (InputStreamReader. (FileInputStream. (io/file path)) "UTF-8"))


(defmethod read-file :gzip
  [_ {:keys [path]}]
  (BufferedReader. (InputStreamReader. (GZIPInputStream. (FileInputStream. (io/file path))))))
