(ns cml.core.dataset
  (:import (java.util.regex Pattern)
           (clojure.lang PersistentVector)))

;TODO turn functions into transducers

(defn- file-lines [file]
     (letfn [(helper [rdr]
                  (lazy-seq (if-let [line (.readLine rdr)]
                              (cons line
                                    (helper rdr))
                              (do (.close rdr)
                                  nil))))]
       (helper (clojure.java.io/reader file))))


(defn- zipmap-types [keys vals]
  (loop [map {}
         ks (seq keys)
         vs (seq vals)]
    (if (and ks
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
             (next vs)) map)))


(defn data-frame [^String file-path
                  ^Pattern re
                  ^PersistentVector column-names]
     (map (fn [x] (zipmap column-names
                          (clojure.string/split x re)))
          (file-lines file-path)))


(defn data-frame-types [^String file-path
                        ^Pattern re
                        ^PersistentVector column-names]
  (map (fn [x] (zipmap-types column-names
                             (clojure.string/split x re)))
       (file-lines file-path)))


