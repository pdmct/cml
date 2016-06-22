(ns cml.core.dataset)

;TODO Design what functionality data frame fn will have IE csv to data frame etc
;TODO Design how data frames will be composable with statistical functions



;TODO Have data frame only take a sequence of string splits and have read csv and all ather
;import functions create a sequence of string splits hence making all other extract functions
;composable

(defn data-frame
  ([keys vals]
   (loop [map (transient {})
          ks (seq keys)
          vs (seq vals)]
     (if (and ks vs)
       (recur (assoc! map (first ks)
                         (first vs))
              (next ks)
              (next vs))
       (persistent! map))))
  ([keys vals xform]
   (loop [map (transient {})
          ks (seq keys)
          vs (seq vals)]
     (if (and ks vs)
       (recur (assoc! map (first ks)
                         (xform (first vs)))
              (next ks)
              (next vs))
       (persistent! map)))))


