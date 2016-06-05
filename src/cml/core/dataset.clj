(ns cml.core.dataset
  (:import (java.util.regex Pattern)
           (clojure.lang PersistentVector)))


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


(defn- zipmap-types
  [keys vals]
  (loop [map {}
         ks (seq keys)
         vs (seq vals)]
    (if (and ks
             vs)
      (recur (assoc map
               ((first ks) 0) (cond (= ((first ks)
                                         1) :string)
                                    (first vs)
                                    (= ((first ks)
                                         1) :integer)
                                    (Integer/parseInt (first vs))
                                    (= ((first ks)
                                         1) :long)
                                    (Long/parseLong (first vs))
                                    (= ((first ks)
                                         1) :double)
                                    (Double/parseDouble (first vs))
                                    (= ((first ks)
                                         1) :character)
                                    (.charAt (first vs)
                                             0)
                                    :else (first vs)))
             (next ks)
             (next vs))
      map)))


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
  (map (fn [x] (zipmap-types column-names
                       (split-str x
                                  re)))
       (file-lines file-path)))




(reduce-kv (fn [m k v] (assoc m k (cond (= :int k)
                                        (read-string v)
                                        (= :str k)
                                        v
                                        (= :long k)
                                        (Long/parseLong v)
                                        :else v))) {} {:int "1" :str "banana" :long "24122352521253"})


(reduce-kv (fn [m k v] (assoc m k (cond (= :int (k 1))
                                        (Integer/parseInt v)
                                        (= :str (k 1))
                                        v
                                        (= :long (k 1))
                                        (Long/parseLong v)
                                        :else v))) {} {[:name :string] "Greg", [:age :int] "22", [:salary :long] "231455342"})


(time (data-frame-types adult-data
                        #","
                        [[:age :integer] [:sector :string] [:code :long] [:degree-type :string]
                         [:study-time :integer] [:marital-status :string] [:industry :string]
                         [:family-status :string] [:race :string] [:gender :string] [:n1 :integer]
                         [:n2 :integer]
                         [:n3 :integer] [:country :string] [:salary-range :string]]))

