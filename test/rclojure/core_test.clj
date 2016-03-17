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
  (is (= (into [] (rsum {:coll [1 2 3] :type :int-array})) [6]))
  (is (int-array? (rsum {:coll [1 2 3] :type :int-array})))
  (is (primitive-array? (rsum {:coll [1 2 3] :type :int-array})))
  (is (double-array? (rsum {:coll [1 2 3] :type :double-array}))))


(deftest rabs-test
  (is (= (into [] (rabs {:coll [1 2 -3 -4 5] :type :int-array})) [1 2 3 4 5]))
  (is (int-array? (rabs {:coll [1 2 3] :type :int-array})))
  (is (primitive-array? (rabs {:coll [1 2 3] :type :int-array})))
  (is (double-array? (rabs {:coll [1 2 3] :type :double-array}))))


(deftest rappend-test
  (is (= (into [] (rappend {:coll [1 2 3 4 5] :coll1 [6 7 8 9 10] :type :int-array})) [1 2 3 4 5 6 7 8 9 10]))
  (is (int-array? (rappend {:coll [1 2 3] :coll1 [4 5 6] :type :int-array})))
  (is (primitive-array? (rappend {:coll [1 2 3] :coll1 [4 5 6] :type :int-array})))
  (is (double-array? (rappend {:coll [1 2 3] :coll1 [4 5 6] :type :double-array}))))


(rcat {:coll (range 1 1000) :type :int-array :set {:file "foo.txt" :sep "," :fill true :labels ["APPLE" "CAKE" "BANANA"]}})

