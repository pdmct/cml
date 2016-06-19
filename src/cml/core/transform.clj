(ns cml.core.transform
  (:require [cml.core.utils :refer [zipmap-types]])
  (:import (java.util.regex Pattern)
           (clojure.lang PersistentVector)))


(defn transform-values [f]
  (fn [m]
    (reduce-kv (fn [map key val]
                 (assoc map
                   key
                   (f val))) {} m)))


(defn transform-by-key                                      ;TODO remove closure from functions
  ([k f]
   (fn [m]
     (assoc m k (f (k m)))))
  ([k f x]
   (fn [m]
     (assoc m k (f (k m) x))))
  ([k f x y]
   (fn [m]
     (assoc m k (f (k m) x x y))))
  ([k f x y z]
   (fn [m]
     (assoc m k (f (k m) x x y z))))
  ([k f x y z & more]
   (fn [m]
     (assoc m k (apply f (k m) x x y z more)))))



(defn tokenize
  ([^Pattern re ^String line]
   (clojure.string/split line
                         re))
  ([^Pattern re ^String line ^PersistentVector names]
   (zipmap names
           (clojure.string/split line
                                 re)))
  ([^Pattern re ^String line ^PersistentVector names transform-line]
   (zipmap names
           (clojure.string/split (transform-line line)
                                 re))))


(defn tokenize-type
  ([^Pattern re ^String line ^PersistentVector names-types]
   (zipmap-types names-types
                 (clojure.string/split line
                                       re)))
  ([^Pattern re ^String line ^PersistentVector names-types transform-line]
   (zipmap-types names-types
                 (clojure.string/split (transform-line line)
                                       re))))

