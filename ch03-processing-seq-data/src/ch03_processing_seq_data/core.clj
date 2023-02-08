(ns ch03-processing-seq-data.core
  (:gen-class))

;;; processing sequential data

;; mapping values
(def G 6.674e-11)

(defn semi-major-axis
  "planet's average distance from the star"
  [p]
  (/ (+ (:aphelion p) (:perihelion p)) 2))

(defn mu [mass]
  (* G mass))

(defn orbital-period
  "time it takes for a planet to make a complete orbit arounda mass, in seconds"
  [p mass]
  (* Math/PI 2
     (Math/sqrt (/ (Math/pow (semi-major-axis p) 3)
                   (mu mass)))))

(defn orbital-periods
  "given a collection of planets, and a star, return the orbital periods of every planet"
  [planets star]
  (let [solar-mass (:mass star)]
    (map (fn [planet]
           (orbital-period planet solar-mass))
         planets)))


;; sequence processing
(defn simple-map [f coll]
  (when (seq coll)
    (cons (f (first coll))
          (simple-map f (rest coll)))))

;; tranducers
(defn orbital-period-transformation
  "create a map transformation for planet->orbital period"
  [star]
  (map #(orbital-period % (:mass star))))

(defn orbital-periods-transducer
  [planets star]
  (sequence (orbital-period-transformation star) planets))

(defn orbital-periods-v
  [planets star]
  (into [] (orbital-period-transformation star) planets))

;; reducing to a value

;; reduce total number of moons
(defn total-moons [planets]
  (reduce + 0 (map :moons planets)))

(defn total-moons-tranduce
  [planets]
  (transduce (map :moons) + 0 planets))

(defn find-planet [planets pname]
  (reduce
   (fn [_ planet]
     (when (= pname (:name planet))
       (reduced planet)))
   planets))

;; filtering and removing values
(defrecord Planet
    [name moons volume mass aphelion perihelion])

(defn planet?
  [entity]
  (instance? Planet entity))

(defn total-moons-planet
  [entities]
  (reduce + 0
          (map :moons
               (filter planet? entities))))

(defn total-moons-thread
  [entities]
  (->> entities
       (filter planet?)
       (map :moons)
       (reduce + 0)))

(def moons-transform
  (comp (filter planet?) (map :moons)))

(defn total-moons-transduce
  [entities]
  (transduce moons-transform + 0 entities))

;; take and drop
(defn nth-page
  "return up to page-size results for the nth page of source"
  [source page-size page]
  (->> source
       (drop (* page page-size))
       (take page-size)))

(defn page-and-rest
  [source page-size]
  (split-at page-size source))

;; sorting and duplicate removal
(defn smallest-n
  [planets n]
  (->> planets
       (sort-by :volume)
       (take n)))

;; grouping values
(defn index-planets
  [planets]
  (group-by #(first (:name %)) planets))

(defn has-moons?
  [planets]
  (pos? (:moons planets)))

(defn split-moons
  [planets]
  (group-by has-moons? planets))
