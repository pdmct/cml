(ns rclojure.core.engine
  (:import [org.rosuda.JRI Rengine]))

(def new-thread
  (Rengine. (into-array ["--vanilla"]) false nil))

(defn reval
  [expr]
  (let [re (Rengine/getMainEngine)]
    (try
      (.eval re expr)
      (finally (.end re)))))
