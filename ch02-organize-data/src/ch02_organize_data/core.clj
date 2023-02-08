(ns ch02-organize-data.core
  (:require [medley.core :refer (map-keys)])
  (:import [clojure.lang Counted Indexed ILookup Seqable]
           [java.io Writer])
  (:gen-class))

;;; collect and organize data

;; sorted collections

;; sorting maps based on last name, which is bad
;; because same last name is considered as dupiclate
;; and discarded
(defn compare-authors-badly [s1 s2]
  (compare (:lname s1) (:lname s2)))

(sorted-set-by compare-authors-badly
               {:fname "Jeff" :lname "Smith"}
               {:fname "Bill" :lname "Smith"})
;; => #{{:fname "Jeff", :lname "Smith"}}

;; better
(defn compare-authors [s1 s2]
  (let [c (compare (:lname s1) (:lname s2))]
    (if (zero? c)
      (compare (:fname s1) (:fname s2))
      c)))

(sorted-set-by compare-authors
               {:fname "Jeff" :lname "Smith"}
               {:fname "Bill" :lname "Smith"})
;; => #{{:fname "Bill", :lname "Smith"} {:fname "Jeff", :lname "Smith"}}

;; using juxt
(defn compare-author [s1 s2]
  (letfn [(project-author [author]
            ((juxt :lname :fname) author))]
    (compare (project-author s1) (project-author s2))))

(sorted-set-by compare-author
               {:fname "Jeff" :lname "Smith"}
               {:fname "Bill" :lname "Smith"})
;; => #{{:fname "Bill", :lname "Smith"} {:fname "Jeff", :lname "Smith"}}

;; updating collections

;; fifo
(defn new-orders-vector []
  [])

(defn new-orders-list []
  '())

(defn new-orders-queue []
  clojure.lang.PersistentQueue/EMPTY) ;; more efficient

(defn cook [order]
  (println (str "cooking:" order)))

(defn add-order-vector [orders order] ;; can be used for queue as well
  (conj orders order))

(defn add-order-list [orders order]
  (concat orders (list order)))

(defn cook-order [orders]
  (cook (first orders))
  (rest orders))

(defn cook-order-queue [orders]
  (cook (peek orders))
  (pop orders))

;; bulk import
;; leveraging controlled mutability in importing bulk data for efficiency

(defn import-catalog [data]
  (reduce #(conj %1 %2) [] data))

(defn import-catalog-fast [data] ;; should be faster since involving immutability
  (persistent!
   (reduce #(conj %1 %2) (transient []) data)))

;; updating maps
(def earth {:name "Earth"
            :moons 1
            :volume 1.08321e12 ;; km^3
            :mass 5.97219e24 ;; kg
            :aphelion 152098232 ;; km, farthest from sun
            :perihelion 147098290 ;; km, closest to sun
            })

(update earth :moons inc)
;; => {:name "Earth", :moons 2, :volume 1.08321E12, :mass 5.97219E24, :aphelion 152098232, :perihelion 147098290}

(defn keywordize-entity [entity]
  (map-keys keyword entity))

(keywordize-entity {"name" "Earth"
                    "moon" 1
                    "volume" 1.08321e12
                    "mass" 5.97219e24
                    "aphelion" 152098232
                    "perihelion" 147098290})
;; => {:name "Earth", :moon 1, :volume 1.08321E12, :mass 5.97219E24, :aphelion 152098232, :perihelion 147098290}

;;; accessing collections

(def earth-2 {:name "Earth" :moons 1})

;;; 3 ways to lookup maps
(get earth :name)
;; => "Earth"
(earth :name)
;; => "Earth"
(:name earth)
;; => "Earth"

(def units [:lb :oz :kg])

(some #{:oz} units)
;; => :oz

(defn contains-val?
  [coll val]
  (reduce (fn [ret elem]
            (if (= val elem)
              (reduced true)
              ret))
          false coll))

(contains-val? units :oz)
;; => true

;; building custom collections
;; a pair that can work with seq, count, nth, and get
(deftype Pair [a b]
  Seqable
  (seq [_] (seq [a b]))

  Counted
  (count [_] 2)

  Indexed
  (nth [_ i]
    (case i
      0 a
      1 b
      (throw (IllegalArgumentException.))))
  (nth [this i _] (nth this i))

  ILookup
  (valAt [_ k _]
    (case 0
      0 a
      1 b
      (throw (IllegalArgumentException.))))
  (valAt [this k] (.valAt this k nil)))

;; testing custom pair collection
(def p (->Pair :r :z))

(seq p)
;; => (:r :z)

(count p)
;; => 2

(nth p 1)
;; => :z

(get p 0)
;; => :r

p
;; => #object[ch02_organize_data.core.Pair 0x41e54a1 "ch02_organize_data.core.Pair@41e54a1"]

;; custom printing for types
(defmethod print-method Pair
  [pair ^Writer w]
  (.write w "#custom.Pair")
  (print-method (vec (seq pair)) w))

(defmethod print-dup Pair
  [pair w]
  (print-method pair w))

(->Pair 3 4)
;; => #custom.Pair[3 4]

#custom.Pair[4 5]
;; => 
