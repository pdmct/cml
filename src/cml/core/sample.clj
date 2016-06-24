(ns cml.core.sample)


#_(defn random-dummy-sample
  ([population]
   (simple/sample population))
  ([population size]
    (stream/sample (range) size population)))


(defn random-population-sample [population size]            ;TODO use java api
  (let [x (atom []) p (atom population) s (atom size)]
    (while (not= @s 0)
      (do
        (swap! x conj (last (swap! p shuffle)))
        (swap! p pop)
        (swap! s dec))) @x))


