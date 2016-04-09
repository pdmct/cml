(ns cml.core.utils.stats)


(defn mean [sample] (quot (reduce + sample) (count sample)))


