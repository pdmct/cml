(ns cml.core.dataset
  (:require [cml.core.utils :refer [zip]]
            [cml.core.transform :refer [line-split]]
            [cml.core.extract :refer [file-lines]]
            [cml.core.utils.file.cell :refer  [read-line-values write-line-values]]
            [cml.core.utils.file.excel.workbook :refer [get-workbook-sheet make-workbook-map write-workbook create-workbook-object create-sheet get-all-sheets]]))

;TODO Design how data frames will be composable with statistical functions
;TODO Implementparallel data frame

(defmulti data-frame (fn [type] (:type type)))


(defn- xform-csv
  ([column-names delim]
   (comp (map #(line-split % delim))
         (map #(zip column-names %))))
  ([column-names delim xform]
   (comp (map #(line-split % delim))
         (map #(zip column-names % xform)))))


(defmethod data-frame :csv/read [type]
  (if-not (:xform type)
    (transduce (xform-csv (:column-names type) (:delimiter type))
               conj (:return type)
               (file-lines (:file-path type)))
    (transduce (xform-csv (:column-names type) (:delimiter type) (:xform type))
               conj (:return type)
               (file-lines (:file-path type)))))

(defmethod data-frame :csv/write [type] )                   ;TODO will take a data frame and write to csv delimited by ?

