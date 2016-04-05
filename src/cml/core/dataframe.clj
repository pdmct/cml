(ns cml.core.dataframe
  (:require [cml.core.file :as file]
            [clojure.java.io :as io])
  (:import [com.datumbox.framework.common Configuration]
           [com.datumbox.framework.common.dataobjects TypeInference$DataType Dataframe$Builder]
           (java.util LinkedHashMap))
  (import com.datumbox.framework.core.machinelearning.datatransformation.XMinMaxNormalizer)
  (import com.datumbox.framework.core.machinelearning.common.abstracts.datatransformers.AbstractDummyMinMaxTransformer$TrainingParameters))




(defn config [] (Configuration/getConfiguration))

(defn header-types []
  (let [lh (LinkedHashMap.)]
    (doto  lh (.put "pregnancies" (TypeInference$DataType/NUMERICAL))
              (.put "plasma glucose" (TypeInference$DataType/NUMERICAL))
              (.put "blood pressure" (TypeInference$DataType/NUMERICAL))
              (.put "triceps thickness" (TypeInference$DataType/NUMERICAL))
              (.put "serum insulin" (TypeInference$DataType/NUMERICAL))
              (.put "bmi" (TypeInference$DataType/NUMERICAL))
              (.put "dpf" (TypeInference$DataType/NUMERICAL))
              (.put "age" (TypeInference$DataType/NUMERICAL))
              (.put "test result" (TypeInference$DataType/CATEGORICAL)))))

(defn dataframe [read-file header-types] (Dataframe$Builder/parseCSVFile read-file "Class"  header-types \, \" "\r\n" nil nil (config)))


(defn training-dataframe [] (dataframe (file/read-file {:file :csv} {:path (io/resource "diabetes.tsv")}) (header-types)))


(let [test-dataframe (.copy (training-dataframe))
      data-transformer (.denormalize test-dataframe (.fit_transform (XMinMaxNormalizer. "Diabetes" (config)) (training-dataframe)
                                                                    (AbstractDummyMinMaxTransformer$TrainingParameters)))]
  data-transformer)
