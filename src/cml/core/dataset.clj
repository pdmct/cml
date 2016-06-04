(ns cml.core.dataset
  (:import (java.util.regex Pattern)))

(defn split-str {:private true}
  ([^CharSequence s ^Pattern re]
   (lazy-seq (.split re s))))


(defn- file-lines [file]
     (letfn [(helper [rdr]
                  (lazy-seq
                       (if-let [line (.readLine rdr)]
                            (cons line (helper rdr))
                            (do (.close rdr) nil))))]
          (helper (clojure.java.io/reader file))))

(defn data-frame [^String file-path
                  ^String delimiter
                  ^clojure.lang.PersistentVector column-names
                  ^clojure.lang.PersistentVector types]
     (map (fn [x] (zipmap column-names
                          (split-str x
                                     delimiter)))
          (file-lines file-path)))


