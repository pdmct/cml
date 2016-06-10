(ns cml.core.transform)



(reduce-kv (fn [m k v] (assoc m k (cond (= :int (k 1))
                                        (Integer/parseInt v)
                                        (= :str (k 1))
                                        v
                                        (= :long (k 1))
                                        (Long/parseLong v)
                                        :else v))) {} {[:name :string] "Greg", [:age :int] "22", [:salary :long] "231455342"})

