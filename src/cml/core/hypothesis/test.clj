(ns cml.core.hypothesis.test
  (:require [cml.core.utils.stats ]))



(defn significance [correlation sample-size]
  (/ (* correlation (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))


(defn t-test [population sample]
  ())


