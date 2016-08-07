(ns cml.core.inference.hypothesis.significance
  (:require [cml.inference.hypothesis.critical-value :refer [significance]])
  (:import [cml.inference.hypothesis.critical_value OneTail TwoTail]))


(defn one-tail-sig-test [{:keys [dof alpha]}] (significance (OneTail. dof alpha)))


(defn two-tail-sig-test [{:keys [dof alpha]}] (significance (TwoTail. dof alpha)))


