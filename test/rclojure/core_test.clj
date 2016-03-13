(ns rclojure.core-test
  (:refer-clojure :exclude [remove cat])
  (:require [clojure.test :refer :all]
            [rclojure.core.rfn :refer [cat]]))

(rsum {:coll [1 2 3] :type :int-array :set {:rm-na? true}})

(into [] (rsum {:coll [1 2 3] :type :int-array}))