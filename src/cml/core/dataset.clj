(ns cml.core.dataset
  (:require [cml.core.utils :refer [zip]]
            [cml.core.transform :refer [line-split]]
            [cml.core.extract :refer [file-lines]])
  (:import
    (org.apache.poi.ss.usermodel Cell Row Sheet Workbook WorkbookFactory)
           (java.io FileInputStream)))

;TODO Design how data frames will be composable with statistical functions


(defmulti data-frame (fn [type] (:type type)))

(defmethod data-frame :csv [type]
  (eduction
    (if-not (:xform type)
      (map #(zip (:column-names type) %))
      (map #(zip (:column-names type) % (:xform type))))
    (map #(line-split % (:delimiter type))
         (file-lines (:file-path type)))))

#_(defmethod data-frame :csv [type]
  (let [workbook (HSSFWorkbook. (FileInputStream. (:file-path type)))
        worksheet (.getSheet workbook (:work-sheet type))
        ritr (.rowIterator worksheet)]
    (while  (.hasNext ritr)
      (while ))))

(defmulti pdata-frame (fn [type] (:type type)))

(defmethod pdata-frame :csv [type]
  (if-not (:xform type)
    (pmap #(zip (:column-names type) %)
          (pmap #(line-split % (:delimiter type))
                (file-lines (:file-path type))))
    (pmap #(zip (:column-names type) % (:xform type))
          (pmap #(line-split % (:delimiter type))
                (file-lines (:file-path type))))))



