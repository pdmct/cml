(ns cml.core.file
  (:require [clojure.java.io :as io])
  (:import [com.datumbox.framework.common Configuration]
           [com.datumbox.framework.common.dataobjects Dataframe Record TypeInference]
           [com.datumbox.framework.common.utilities PHPMethods RandomGenerator]
           [com.datumbox.framework.core.machinelearning.classification SoftMaxRegression]
           [com.datumbox.framework.core.machinelearning.datatransformation XMinMaxNormalizer]
           [com.datumbox.framework.core.machinelearning.featureselection.continuous PCA]
           (java.io BufferedReader InputStreamReader FileInputStream File)
           (java.util.zip GZIPInputStream)
           (java.nio.file Paths)))


(defn read-file
  [file]
  (try (InputStreamReader. (FileInputStream. (io/file file)))))


