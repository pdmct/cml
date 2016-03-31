(ns rclojure.core-test
  (:refer-clojure :exclude [remove cat])
  (:require [clojure.test :refer :all]
            [rclojure.core :refer :all]))


(defn primitive-array? [o]
  (let [c (class o)]
    (and (.isArray c)
         (.. c getComponentType isPrimitive))))


(defn int-array? [o]
  (let [c (class o)]
    (and (.isArray c)
         (identical? (.getComponentType c) Integer/TYPE))))


(defn double-array? [o]
  (let [c (class o)]
    (and (.isArray c)
         (identical? (.getComponentType c) Double/TYPE))))


(deftest rsum-test
  (is (= (into [] (rsum [1 2 3])) [6]))
  (is (seq? (rsum [1 2 3]))))


(deftest rabs-test
  (is (= (into [] (rabs [1 2 -3 -4 5])) [1 2 3 4 5]))
  (is (seq? (rabs [1 2 3]))))


(deftest rappend-test
  (is (= (into [] (rappend [1 2 3 4 5] [6 7 8 9 10])) [1 2 3 4 5 6 7 8 9 10]))
  (is (seq? (rappend (int-array [1 2 3]) (int-array [4 5 6])))))


(rcat {:coll (range 1 1000)
           :set {:file "foo.txt"
                   :sep    ","
                   :fill   true
                   :labels ["APPLE" "CAKE" "BANANA"]}})


(rmatrix {:coll [1 2 3 11 12 13]
          :set  {
                 :nrow  2
                 :ncol  3
                 :byrow true
                 :rows  ["row1" "row2"]
                 :cols  ["C.1" "C.2" "C.3"]}})


(rplot-matrix {:coll [1 2 3 11 12 13]
               :file :jpg
               :set  {
                      :fname "/Users/gra11/lemon1.jpg"
                      :nrow  2
                      :ncol  3
                      :byrow true
                      :rows  ["row1" "row2"]
                      :cols  ["C.1" "C.2" "C.3"]}})


