(ns ch09-format.core
  (:require [clojure.edn :as edn]
            [clojure.data.json :as j]
            [cheshire.core :refer :all]
            [cheshire.generate :refer [add-encoder]]
            [cognitect.transit :as transit])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream])
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

;; json

;; using data.json
(j/read-str "[{\"name\" : \"Ben\", \"age\" :39}, {\"name\": \"Monster\", \"age\": 4}]")
;; => [{"name" "Ben", "age" 39} {"name" "Monster", "age" 4}]

(j/write-str [{:name "Riz"}, {:name "Mina" :age 3}])
;; => "[{\"name\":\"Riz\"},{\"name\":\"Mina\",\"age\":3}]"

;; using cheshire
(parse-string "[{\"name\" : \"Ben\", \"age\" :39}, {\"name\": \"Monster\", \"age\": 4}]")
;; => ({"name" "Ben", "age" 39} {"name" "Monster", "age" 4})

(generate-string [{:name "Riz"}, {:name "Mina" :age 3}])
;; => "[{\"name\":\"Riz\"},{\"name\":\"Mina\",\"age\":3}]"

(parse-string "[{\"name\" : \"Ben\", \"age\" :39}, {\"name\": \"Monster\", \"age\": 4}]" true)
;; => ({:name "Ben", :age 39} {:name "Monster", :age 4})

(generate-string {:date (java.util.Date.)})
;; => "{\"date\":\"2023-02-20T03:45:56Z\"}"

(def ^:private date-format
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSSXXX")))))

(defn- format-inst
  "create a tolerable string from an inst"
  [d]
  (str "#inst (.format (.get date-format) d)"))

(defn- date-part
  "extract the date part of a stringified #inst"
  [d]
  (second (re-matches #"#inst (.*)" d)))

(add-encoder java.util.Date
             (fn [d generator]
               (.writeString generator (format-inst d))))

(generate-string {:date (java.util.Date.)})
;; => "{\"date\":\"#inst (.format (.get date-format) d)\"}"

(defn read-date [k v]
  (if (= :date k)
    (.parse (.get date-format) (date-part v))
    v))

(defn write-date [k v]
  (if (= :date k)
    (str "#inst " (.format (.get date-format) v))
    v))

(j/write-str {:keyword "string"
              :date (java.util.Date.)}
              :value-fn write-date)
;; => "{\"keyword\":\"string\",\"date\":\"#inst 2023-02-20T11:11:24.843+07:00\"}"

(j/read-str
 (j/write-str {:keyword "string"
               :date (java.util.Date.)}
              :value-fn write-date)
 :key-fn keyword
 :value-fn read-date)
;; => {:keyword "string", :date #inst "2023-02-20T04:12:18.546-00:00"}

;;; transit
(def output (ByteArrayOutputStream. 4096))
(def writer (transit/writer output :json-verbose))

(transit/write writer [
                       {:name "joe smith"
                        :email "joe@company.com"
                        :roles [:admin :supervisor :analyst]}
                       {:name "robert jones"
                        :email "rob@company.com"
                        :roles [:analyst]}
                       ])
;; => nil

(declare validate-same-currency)

(defrecord Currency [divisor sym desc])

(defrecord Money [amount ^Currency currency]
  java.lang.Comparable
  (compareTo [m1 m2]
    (validate-same-currency m1 m2)
    (compare (:amount m1) (:amount m2))))

(def currencies {:usd (->Currency 100 "USD" "US Dollars")
                 :eur (->Currency 100 "EUR" "Euro")})

(defn- validate-same-currency
  [m1 m2]
  (or (= (:currency m1) (:currency m2))
      (throw
       (ex-info "Currencies do not match."
                {:m1 m1 :m2 m2}))))

(def write-handlers {
                     Currency
                     (reify WriteHandler
                       (tag [_ _] "currency")
                       (rep [_ c] [(:divisor c) (:sym c) (:desc c)])
                       (stringRep [_ _] nil)
                       (getVerboseHandler [_] nil))
                     Money
                     (reify WriteHandler
                       (tag [_ m] "money")
                       (rep [_ m] [(:amount m) (:currency m)])
                       (stringRep [_ _] nil)
                       (getVerboseHandler [_] nil))})
