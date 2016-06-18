(ns cml.core.dataset
  (:import (java.util.regex Pattern)
           (clojure.lang PersistentVector))
  (:require [cml.core.utils :refer [zipmap-types]]))

;TODO continue adding extra arity for functions

(defn file-lines [file]
     (letfn [(helper [rdr]
                  (lazy-seq (if-let [line (.readLine rdr)]
                              (cons line
                                    (helper rdr))
                              (do (.close rdr)
                                  nil))))]
       (helper (clojure.java.io/reader file))))

(defn splitter                                              ;TODO Can rename splitter to data-frame
  ([^PersistentVector column-names]
   (fn [x]
     (zipmap column-names
             (clojure.string/split x
                                   #","))))
  ([^PersistentVector column-names ^Pattern re]
   (fn [x]
     (zipmap column-names
             (clojure.string/split x
                                   re))))
  ([^PersistentVector column-names ^Pattern re xform-fn]
   (fn [x]
     (zipmap column-names
             (xform-fn (clojure.string/split x
                                             re))))))

(defn data-frame [^String file-path
                  ^Pattern re
                  ^PersistentVector column-names] ;TODO pull map outside function
     (map (fn [x] (zipmap column-names
                          (clojure.string/split x re)))
          (file-lines file-path)))


(defn data-frame-types
  ([^String file-path
    ^PersistentVector column-names]                         ;TODO pull map outside function
   (map (fn [x] (zipmap-types column-names
                              (clojure.string/split x #",")))
        (file-lines file-path)))
  ([^String file-path
    ^Pattern re
    ^PersistentVector column-names]
   (map (fn [x] (zipmap-types column-names
                              (clojure.string/split x re)))
        (file-lines file-path))))



