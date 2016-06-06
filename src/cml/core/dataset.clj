(ns cml.core.dataset
  (:import (java.util.regex Pattern)
           (clojure.lang PersistentVector)))

;TODO turn functions into transducers

(defn- split-str [^CharSequence s ^Pattern re]
  (lazy-seq (.split re
                    s)))


(defn- file-lines [file]
     (letfn [(helper [rdr]
                  (lazy-seq (if-let [line (.readLine rdr)]
                              (cons line
                                    (helper rdr))
                              (do (.close rdr)
                                  nil))))]
       (helper (clojure.java.io/reader file))))


(defn data-frame [^String file-path
                  ^Pattern re
                  ^PersistentVector column-names]
     (map (fn [x] (zipmap column-names
                          (split-str x
                                     re)))
          (file-lines file-path)))


(defn data-frame-types [^String file-path
                        ^Pattern re
                        ^PersistentVector column-names]
  (letfn [(zipmap-types [keys vals]
            (loop [map {}
                   ks (seq keys)
                   vs (seq vals)]
              (if (and ks
                       vs)
                (recur (assoc map
                         ((first ks) 0)
                         (cond (= (second (first ks))
                                  :string)
                               (first vs)
                               (= (second (first ks))
                                  :integer)
                               (Integer/parseInt (first vs))
                               (= (second (first ks))
                                  :long)
                               (Long/parseLong (first vs))
                               (= (second (first ks))
                                  :double)
                               (Double/parseDouble (first vs))
                               (= (second (first ks))
                                  :character)
                               (.charAt (first vs) 0)
                               :else (first vs)))
                       (next ks)
                       (next vs)) map)))]
    (map (fn [x] (zipmap-types column-names
                               (split-str x
                                          re)))
         (file-lines file-path))))


