(ns cml.core.dataset
  (:import [net.sf.javaml.core Dataset]
           [net.sf.javaml.tools.data FileHandler]
           [java.io File]))

(defn load-dataset
  [{:keys [file index sep]}]
  (FileHandler/loadDataset (File. file) index sep))


