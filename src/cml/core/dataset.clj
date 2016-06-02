(ns cml.core.dataset)

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
                          (clojure.string/split x
                                                delimiter)))
          (file-lines file-path)))


