(ns ch01-domain.core
  (:require [ch01-domain.money :as money])
  (:gen-class))

;;;; modeling your domain

;; using map
(def earth {:name "Earth"
            :moons 1
            :volume 1.08321e12 ;; km^3
            :mass 5.97219e24 ;; kg
            :aphelion 152098232 ;; km, farthest from sun
            :perihelion 147098290 ;; km, closest to sun
            :type :Playe ;; entity type
            })

;; using record
(defrecord Planet [name
                   moons
                   volume
                   mass
                   aphelion
                   perihelion])

;; positional factory fn
(def earth-record-1
  (->Planet "Earth" 1 1.08321e12 5.97219e24 152098232 147098290))

;; map factory fn
(def earth-record-2
  (map->Planet earth))
