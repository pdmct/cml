(ns cml.core.transform
  (:require [cml.core.utils :refer [zipmap-types]])
  (:import (java.util StringTokenizer)))


(defn transform-values [f]                                  ;TODO Use assoc instead of reduce-kv
  (fn [m]
    (reduce-kv (fn [map key val]
                 (assoc map
                   key
                   (f val))) {} m)))


(defn transform-by-key
  ([map key transform-fn]
   (assoc map
     key (transform-fn (get map
                            key))))
  ([map key transform-fn args2]
   (assoc map
     key
     (transform-fn (get map
                        key)
                   args2)))
  ([map key transform-fn args2 args3]
   (assoc map
     key
     (transform-fn (get map
                        key)
                   args2 args3)))
  ([map key transform-fn args2 args3 args4]
   (assoc map
     key
     (transform-fn (get map
                        key)
                   args2 args3 args4)))
  ([map key transform-fn args2 args3 args4 & more]
   (assoc map
     key
     (apply transform-fn
            (get map
                 key)
            args2 args3 args4 more))))


(defn splitter
  ([^String line ^String delimiter]
   (let [st (StringTokenizer. line delimiter)
         col (transient [])]
     (while (.hasMoreTokens st)
       (conj! col (.nextToken st)))
     (persistent! col)))
  ([^String line ^String delim ^Boolean keep-delims?]
   (let [st (StringTokenizer. line delim keep-delims?)
         col (transient [])]
     (while (.hasMoreTokens st)
       (conj! col (.nextToken st)))
     (persistent! col))))


