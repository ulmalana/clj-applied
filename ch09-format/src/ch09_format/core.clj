(ns ch09-format.core
  (:require [clojure.edn :as edn])
  (:gen-class))

;; edn
(def users (atom []))

(defn init-users []
  (reset! users
          (edn/read-string (slurp "resources/users.edn"))))


(init-users)
;; => [{:name "Joe Smith", :email "joe@company.com", :roles [:admin :supervisor :analyst]} {:name "Robert Jones", :email "rob@company.com", :roles [:analyst]}]

;; extending edn with literal tags
(defrecord Card [rank suit])

(def ranks "23456789TJQKA")
(def suits "hdcs")

(defn- check [val vals]
  (if (some #{val} (set vals))
    val
    (throw (IllegalArgumentException.
            (format "Invalid value: %s, expected %s" val vals)))))

(defn card-reader [card]
  (let [[rank suit] card]
    (->Card (check rank ranks) (check suit suits))))
;; => #'ch09-format.core/card-reader

(card-reader "2c")
;; => #ch09_format.core.Card{:rank \2, :suit \c}

(binding [*data-readers* {'my/card #'ch09-format.core/card-reader}]
  (read-string "#my/card \"2c\""))
;; => #ch09_format.core.Card{:rank \2, :suit \c}

(defn card-str [card]
  (str "#my/card \"" (:rank card) (:suit card) "\""))

(defmethod print-method ch09_format.core.Card [card ^java.io.Writer w]
  (.write w (card-str card)))

(defmethod print-dup ch09_format.core.Card [card w]
  (print-method card w))

(defn write-text-to-file [text f]
  (with-open [w (clojure.java.io/writer f)]
    (.write w text)))
