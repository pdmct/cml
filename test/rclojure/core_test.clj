(ns rclojure.core-test
  (:refer-clojure :exclude [remove cat])
  (:require [clojure.test :refer :all]
            [rclojure.core.rfn :refer [cat]]))


(deftest cat-test
  (is (= (cat [1 2 3] {:file "foo.txt"}) "c(1,2,3)")))

(defn rcat
  ([coll]
   (let [gs (str (gensym))]
     (try
       (rassign gs (double-array coll))
       (reval (str "cat(c(1,2,3,4,5), file = \"/Users/gra11/too33.txt\")"))))))
