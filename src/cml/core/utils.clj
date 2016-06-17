(ns cml.core.utils)


(defn zipmap-types [keys vals]                             ;TODO Add the abaility to add a transformation fn via (comp (Integr/parseInt) (xform ..) (xform.. )
  (loop [map (transient {})
         ks (seq keys)
         vs (seq vals)]
    (if (and (apply hash-map
                    ks)
             vs)
      (recur (assoc map
               (first ks)
               (cond (= (second ks)
                        :string)
                     (first vs)
                     (=  (second ks)
                         :integer)
                     (Integer/parseInt (first vs))
                     (= (second ks)
                        :long)
                     (Long/parseLong (first vs))
                     (= (second ks)
                        :double)
                     (Double/parseDouble (first vs))
                     (= (second ks)
                        :character)
                     (.charAt (first vs) 0)
                     :else (first vs)))
             (drop 2 ks)
             (next vs)) (persistent! map))))


