(ns cml.core.utils.file.excel
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.xml :as x])
  (:import (org.apache.poi.ss.usermodel Cell Row Sheet Workbook WorkbookFactory)
           (org.apache.poi.ss.util NumberToTextConverter)))

(defn load-workbook
  "Load a workbook from a string path."
  [path]
   (WorkbookFactory/create (clojure.java.io/input-stream path)))

(defn read-sheet
  ([workbook] (read-sheet workbook "Sheet1"))
  ([workbook sheet-name]
   (let [sheet   (.getSheet workbook sheet-name)
         rows    (iterator-seq (.iterator sheet))]
     rows)))

(defn read-sheet-test
  ([workbook] (read-sheet workbook "Sheet1"))
  ([workbook sheet-name]
   (let [sheet   (.getSheet workbook sheet-name)]
     (while (println  (seq (.rowIterator sheet)))))))

;TODO wrap poi api in protocols



(defn read-sheet-test
  ([workbook] (read-sheet workbook "Sheet1"))
  ([workbook sheet-name]
   (let [sheet   (.getSheet workbook sheet-name)
         rows    (iterator-seq (.rowIterator sheet))]
     (map (fn [x]
            (if (nil? (.getCell x 1 Row/RETURN_BLANK_AS_NULL))
              nil
              (if (= Cell/CELL_TYPE_NUMERIC (.getCellType (.getCell x 1)))
                (NumberToTextConverter/toText (.getNumericCellValue (.getCell x 1)))
                (.getStringCellValue (.getCell x 1)))))
          rows))))


(rest (map (fn [y] (mapcat :content (mapcat :content (:content (x/parse-str (str y))))))
           (read-sheet (load-workbook "/Users/gra11/Downloads/bank.xlsx") "D6")))