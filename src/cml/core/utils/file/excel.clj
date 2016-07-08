(ns cml.core.utils.file.excel
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.xml :as x])
  (:import (org.apache.poi.ss.usermodel Cell Row Sheet Workbook WorkbookFactory)
           (org.apache.poi.ss.util NumberToTextConverter CellReference)))

(defn load-workbook
  "Load a workbook from a string path."
  [path]
   (WorkbookFactory/create (clojure.java.io/input-stream path)))


(defn read-sheet
  ([workbook] (read-sheet workbook "Sheet1"))
  ([workbook sheet-name]
   (let [sheet   (.getSheet workbook sheet-name)
         rows    (iterator-seq (.rowIterator sheet))]
     (for [y (range 0 (.getLastRowNum sheet))]
       (map (fn [x]
              (cond (nil? (.getCell x y Row/RETURN_BLANK_AS_NULL))
                    nil
                    (= Cell/CELL_TYPE_NUMERIC (.getCellType (.getCell x y)))
                    (NumberToTextConverter/toText (.getNumericCellValue (.getCell x y)))
                    :else (.getStringCellValue (.getCell x y))))
            (rest rows))))))


(defn read-sheet
  ([workbook] (read-sheet workbook "Sheet1"))
  ([workbook sheet-name]
   (let [sheet   (.getSheet workbook sheet-name)
         rows    (iterator-seq (.rowIterator sheet))
         cells  (map (fn [x] (.toString(.cellIterator x))) rows)]
     cells)))

