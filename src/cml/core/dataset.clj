(ns cml.core.dataset
  (:require [cml.core.utils :refer [zip]]
            [cml.core.transform :refer [line-split]]
            [cml.core.extract :refer [file-lines]])
  (:import (org.apache.poi.hssf.usermodel HSSFCell HSSFRow HSSFSheet
                                          HSSFWorkbook)
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

(defmulti pdata-frame (fn [type] (:type type)))

(defmethod pdata-frame :csv [type]
  (if-not (:xform type)
    (pmap #(zip (:column-names type) %)
          (pmap #(line-split % (:delimiter type))
                (file-lines (:file-path type))))
    (pmap #(zip (:column-names type) % (:xform type))
          (pmap #(line-split % (:delimiter type))
                (file-lines (:file-path type))))))


