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
  (is (= (into [] (rsum {:coll (int-array [1 2 3]) :type :int-array})) [6]))
  (is (seq? (rsum {:coll (int-array [1 2 3]) :type :int-array})))
  (is (seq? (rsum {:coll (int-array [1 2 3]) :type :int-array})))
  (is (seq? (rsum {:coll (int-array [1 2 3]) :type :double-array}))))


(deftest rabs-test
  (is (= (into [] (rabs {:coll (int-array [1 2 -3 -4 5]) :type :int-array})) [1 2 3 4 5]))
  (is (seq? (rabs {:coll (int-array [1 2 3]) :type :int-array})))
  (is (seq? (rabs {:coll (int-array [1 2 3]) :type :int-array})))
  (is (seq? (rabs {:coll (int-array [1 2 3]) :type :double-array}))))


(deftest rappend-test
  (is (= (into [] (rappend {:coll (int-array [1 2 3 4 5]) :coll1 (int-array [6 7 8 9 10]) :type :int-array})) [1 2 3 4 5 6 7 8 9 10]))
  (is (seq? (rappend {:coll (int-array [1 2 3]) :coll1 (int-array [4 5 6]) :type :int-array})))
  (is (seq? (rappend {:coll (int-array [1 2 3]) :coll1 (int-array [4 5 6]) :type :int-array})))
  (is (seq? (rappend {:coll (int-array [1 2 3]) :coll1 (int-array [4 5 6]) :type :double-array}))))


(rcat {:coll (range 1 1000)
       :type :int-array
       :set  {:file   "foo.txt"
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
                      :fname "/Users/gra11/lemon3.jpg"
                      :nrow  2
                      :ncol  3
                      :byrow true
                      :rows  ["row1" "row2"]
                      :cols  ["C.1" "C.2" "C.3"]}})

;(.assign (engine "--vanilla") "x" (eval-expr "as.matrix(c(1,2,3))"))
;(eval-expr "mdat <- matrix(c(1,2,3, 11,12,13), nrow = 12, ncol = 7, byrow = TRUE)")
; (defn foo [x] (second (re-find #"\$(.*)$" (str (class foo)))))
;(defmacro <- [binding] `(~'def 'binding (second (re-find #"\$(.*)$" (str (class foo))))))


;(defmacro <- [binding expr] `(~'def 'binding (~'eval-expr (~'str 'binding "<-" ~'expr))))
;(eval-expr "mdat <- matrix(c(1,2,3, 11,12,13), nrow = 12, ncol = 7, byrow = TRUE)")