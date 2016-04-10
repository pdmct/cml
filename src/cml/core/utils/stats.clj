(ns cml.core.utils.stats)


(defn mean [sample] (quot (reduce + sample) (count sample)))

(defn coefficient-determination [rho] (* rho rho))


