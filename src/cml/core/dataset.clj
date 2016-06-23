(ns cml.core.dataset
  (:require [cml.core.utils :refer [zip]]
            [cml.core.transform :refer [line-split]]
            [cml.core.extract :refer [file-lines]]))

;TODO Design how data frames will be composable with statistical functions


(defmulti data-frame (fn [type] (:type type)))

(defmethod data-frame :csv [type]
  (if-not (:parallel type)
    (eduction
      (if-not (:xform type)
        (map #(zip (:column-names type) %))
        (map #(zip (:column-names type) % (:xform type))))
      (map #(line-split % (:delimiter type))
           (file-lines (:file-path type))))
    (if-not (:xform type)
      (pmap #(zip (:column-names type) %)
            (pmap #(line-split % (:delimiter type))
                  (file-lines (:file-path type))))
      (pmap #(zip (:column-names type) % (:xform type))
            (pmap #(line-split % (:delimiter type))
                  (file-lines (:file-path type)))))))


