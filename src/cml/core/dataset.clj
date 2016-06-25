(ns cml.core.dataset
  (:require [cml.core.utils :refer [zip]]
            [cml.core.transform :refer [line-split]]
            [cml.core.extract :refer [file-lines]])
  (:import (org.apache.poi.ss.usermodel Cell Row Sheet Workbook WorkbookFactory)
           (java.io FileInputStream)))

;TODO Design how data frames will be composable with statistical functions
;TODO Implementparallel data frame

(defmulti data-frame (fn [type] (:type type)))


(defn xform-csv
  ([column-names delim]
   (comp
     (map #(line-split % delim))
     (map #(zip column-names %))))
  ([column-names delim xform]
   (comp
     (map #(line-split % delim))
     (map #(zip column-names % xform)))))


(defmethod data-frame :csv [type]
           (transduce (xform-csv (:column-names type) (:delimiter type))
                      conj (:return type)
                      (file-lines (:file-path type))))


(defmethod data-frame :csv/xform [type]
  (transduce (xform-csv (:column-names type) (:delimiter type) (:xform type))
             conj (:return type)
             (file-lines (:file-path type))))


