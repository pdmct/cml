(ns cml.core.transform.string
  (:import (com.google.common.base Splitter)))

(defn splitter [^String line delim]
  (condp instance? delim
    java.lang.Character
    (iterator-seq (.iterator (.split (Splitter/on delim) line)))
    java.lang.Long
    (iterator-seq (.iterator (.split (Splitter/fixedLength delim) line)))
    java.lang.String
    (iterator-seq (.iterator (.split (Splitter/onPattern delim) line)))))


