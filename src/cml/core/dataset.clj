(ns cml.core.dataset
  (:require [cml.core.utils :refer [zip]]
            [cml.core.extract :refer [file-lines]]
            [cml.core.transform.string :refer [splitter]]))

;TODO Design how data frames will be composable with statistical functions
;TODO Implementparallel data frame


(defmulti data-frame (fn [type] (:type type)))


(defn- xform-csv
  ([column-names delim]
   (comp (map #(splitter % delim))
         (map #(zip column-names %))))
  ([column-names delim xform]
   (comp (map #(splitter % delim))
         (map #(zip column-names % xform)))))


(defmethod data-frame :csv/read [type]
  (transduce (xform-csv (:column-names type) (:delimiter type) (:xform type))
             conj (:return type)
             (file-lines (:file-path type))))

(defmethod data-frame :csv/write [type] )                   ;TODO will take a data frame and write to csv delimited by ?

