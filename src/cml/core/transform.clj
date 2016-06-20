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
  ([key transform-fn]
   (fn [map]
     (assoc map
       key (transform-fn (get map
                              key)))))
  ([key transform-fn args2]
   (fn [map]
     (assoc map
       key (transform-fn (get map
                              key) args2))))
  ([key transform-fn args2 args3]
   (fn [map]
     (assoc map
       key (transform-fn (get map
                              key) args2 args3))))
  ([key transform-fn args2 args3 args4]
   (fn [map]
     (assoc map
       key (transform-fn (get map
                              key) args2 args3 args4))))
  ([key transform-fn args2 args3 args4 & more]
   (fn [map]
     (assoc map
       key (apply transform-fn
                  (get map
                       key) args2 args3 args4 more)))))


(defn tokenize-line
  ([^Pattern re ^String line]
   (clojure.string/split line
                         re))
  ([^Pattern re ^String line ^PersistentVector names]
   (zipmap names
           (clojure.string/split line re)))
  ([^Pattern re ^String line ^PersistentVector names transform-line]
   (zipmap names
           (clojure.string/split (transform-line line) re))))


(defn tokenize-line-type
  ([^Pattern re ^String line ^PersistentVector names-types]
   (zipmap-types names-types
                 (clojure.string/split line
                                       re)))
  ([^Pattern re ^String line ^PersistentVector names-types transform-line]
   (zipmap-types names-types
                 (clojure.string/split (transform-line line) re))))


