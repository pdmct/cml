(ns cml.core.correlation)

(def sample {:x [490 500 530 550 580 590 600 600 650 700]
             :y [560 500 510 600 600 620 550 630 650 750 ]})


(defn- mean [sample] (quot (reduce + sample) (count sample)))

(defn- deviation-score [mean sample] (map (fn [x]  (- (mean sample) x)) sample))

(defn- deviation-score-squared [sample] (map (fn [x] (* x x)) (deviation-score mean sample)))

(defn- deviation-score-sum [sample] (reduce + (deviation-score-squared sample)))

(defn pearson-correlation [{:keys [x y]}]
  (/ (reduce + (map * (deviation-score mean x) (deviation-score mean y))) (Math/sqrt (* (deviation-score-sum x) (deviation-score-sum y)))))


