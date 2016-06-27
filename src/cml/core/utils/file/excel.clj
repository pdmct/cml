(ns cml.core.utils.file.excel
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.xml :as x])
  (:import (org.apache.poi.ss.usermodel Cell Row Sheet Workbook WorkbookFactory)
           (java.io File FileInputStream)))


(defn get-cell-string-value
  "Get the value of a cell as a string, by changing the cell type to 'string'
   and then changing it back."
  [cell]
  (let [ct    (.getCellType cell)
        _     (.setCellType cell Cell/CELL_TYPE_STRING)
        value (.getStringCellValue cell)]
    (.setCellType cell ct)
    value))

(defn read-row
  "Read all the cells in a row (including blanks) and return a list of values."
  [row]
  (for [i (range 0 (.getLastCellNum row))]
    (get-cell-string-value (.getCell row (.intValue i)))))

(defn load-workbook
  "Load a workbook from a string path."
  [path]
   (WorkbookFactory/create (clojure.java.io/input-stream path)))

(defn read-sheet
  ([workbook] (read-sheet workbook "Sheet1"))
  ([workbook sheet-name]
   (let [sheet   (.getSheet workbook sheet-name)
         rows    (iterator-seq (.iterator sheet))
         data    (map read-row (rest rows))]
     rows)))


(x/parse-str "<xml-fragment r=\"2\" spans=\"1:27\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" xmlns:x14ac=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac\" xmlns:main=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">
            <main:c r=\"A2\" s=\"1\">
              <main:v>999999999</main:v>
            </main:c>
            <main:c r=\"B2\" s=\"1\">
              <main:v>88888888</main:v>
            </main:c>
            <main:c r=\"C2\" s=\"1\">
              <main:v>3</main:v>
            </main:c>
            <main:c r=\"D2\" s=\"1\">
              <main:v>111111111</main:v>
            </main:c>
            <main:c r=\"E2\" s=\"1\">
              <main:v>44444444</main:v>
            </main:c>
            <main:c r=\"F2\" s=\"1\">
              <main:v>1232123</main:v>
            </main:c>
            <main:c r=\"AA2\" s=\"1\">
              <main:v>22</main:v>
            </main:c>
          </xml-fragment>")


