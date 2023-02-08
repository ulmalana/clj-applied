(ns ch01-domain.recipe
  (:require [schema.core :as s]))

(defrecord Recipe
    [name ;; string
     author ;; creator
     description ;; string
     ingredients ;; list of ingredients
     steps ;; sequence of string
     servings ;; number of servings
     ])

(defrecord Person
    [fname lname])

;; connecting Recipe and Person via nesting
(def toast
  (->Recipe
   "Toast"
   (->Person "Sanji" "Vinsmoke") ;; nested
   "Crispy bread"
   ["Slice of bread"]
   ["Toast bread in toaster"]
   1))

;; connecting Recipe and Person via identifier
(def people
  {"p1" (->Person "Vinsmoke" "Sanji")})

(def recipes
  {"r1" (->Recipe
         "Toast"
         "p1" ;; Person id
         "Crispy bread"
         ["Slice of bread"]
         ["Toast bread in toaster"]
         1)})

;;; validating entities
(defrecord Recipe-2
    [name ;; string
     description ;; string
     ingredients ;; sequence of ingredients
     steps ;; sequence of string
     servings ;; number
     ])

(defrecord Ingredient
    [name ;; string
     quantity ;; amount
     unit ;; keyword
     ])

(def spaghetti-tacos
  (map->Recipe
   {:name "Spaghetti tacos"
    :description "Spaghetti in a taco"
    :ingredients [(->Ingredient "Spaghetti" 1 :lb)
                  (->Ingredient "Spaghetti sauce" 16 :oz)
                  (->Ingredient "Taco shell" 12 :shell)]
    :steps ["Cook spaghetti according to box."
            "Heat spaghetti sauce until warm."
            "mix spaghetti and sauce"
            "put spaghetti in taco shell and serve"]
    :servings 4}))

(s/defrecord Ingredient
    [name :- s/Str
     quantity :- s/Int
     unit :- s/Keyword])

(s/defrecord Recipe
    [name :- s/Str
     description :- s/Str
     ingredients :- [Ingredient]
     steps :- [s/Str]
     servings :- s/Int])

(s/explain Recipe)
;; (record
;;  ch01_domain.recipe.Recipe
;;  {:name Str,
;;   :description Str,
;;   :ingredients
;;   [(record ch01_domain.recipe.Ingredient
;;            {:name Str,
;;             :quantity Int,
;;             :unit Keyword})],
;;   :steps [Str],
;;   :servings Int})

(s/check Recipe spaghetti-tacos)
;; => nil

(s/defn add-ingredients :- Recipe
  [recipe :- Recipe & ingredients :- [Ingredient]]
  (update-in recipe [:ingredients] into ingredients))

(comment
  (clojure.repl/doc add-ingredients)
  -------------------------
  ch01-domain.recipe/add-ingredients
  ([recipe & ingredients])
  Inputs: [recipe :- Recipe & ingredients :- [Ingredient]]
  Returns: Recipe
  nil)
