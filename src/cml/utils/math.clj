(ns cml.utils.math
  (:require [clojure.core.reducers :as r]))


(defn squared
  ([coll] (map #(* % %) coll))
  ([f coll] (map #(* % %) (f coll))))


(defn sum
  ([coll] (reduce + coll))
  ([f coll] (reduce + (f coll))))


