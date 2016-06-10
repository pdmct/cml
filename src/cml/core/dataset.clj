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


(defn update-map ([m f]
                  (reduce-kv (fn [m k v]
                               (assoc m k (f v))) {} m))
  ([m f f1] (reduce-kv (fn [m k v]
                         (assoc m k (f1 (f v)))) {} m)))


(defn -comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  {:added "1.0"
   :static true}
  ([] identity)
  ([f] f)
  ([f g]
   (fn
     ([] (f (g)))
     ([x] (f (g x)))
     ([x y] (f (g x y)))
     ([x y z] (f (g x y z)))
     ([x y z & args] (f (apply g x y z args)))))
  ([f g & fs]
   (reduce comp (list* f g fs))))

