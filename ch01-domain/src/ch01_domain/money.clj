(ns ch01-domain.money)

;;; abstracting currency

(declare validate-same-currency)

(defrecord Currency [divisor sym desc])

(defrecord Money [amount ^Currency currency]
  java.lang.Comparable
  (compareTo [m1 m2]
    (validate-same-currency m1 m2)
    (compare (:amount m1) (:amount m2))))

(def currencies {:usd (->Currency 100 "USD" "US Dollars")
                 :eur (->Currency 100 "EUR" "Euro")})

(defn- validate-same-currency [m1 m2]
  (or (= (:currency m1) (:currency m2))
      (throw
       (ex-info "Currencies dont match"
                {:m1 m1 :m2 m2}))))

(defn =$
  ([m1] true)
  ([m1 m2] (zero? (.compareTo m1 m2)))
  ([m1 m2 & monies]
   (every? zero? (map #(.compareTo m1 %) (conj monies m2)))))

(defn +$
  ([m1] m1)
  ([m1 m2]
   (validate-same-currency m1 m2)
   (->Money (+ (:amount m1) (:amount m2)) (:currency m1)))
  ([m1 m2 & monies]
   (reduce +$ m1 (conj monies m2))))

(defn *$ [m n]
  (->Money (* n (:amount m)) (:currency m)))

(defn make-money
  ([] (make-money 0))
  ([amount] (make-money amount (:usd currencies)))
  ([amount currency] (->Money amount currency)))

(make-money)
;; => #ch01_domain.money.Money{:amount 0, :currency #ch01_domain.money.Currency{:divisor 100, :sym "USD", :desc "US Dollars"}}

(make-money 1)
;; => #ch01_domain.money.Money{:amount 1, :currency #ch01_domain.money.Currency{:divisor 100, :sym "USD", :desc "US Dollars"}}

(make-money 5 (:eur currencies))
;; => #ch01_domain.money.Money{:amount 5, :currency #ch01_domain.money.Currency{:divisor 100, :sym "EUR", :desc "Euro"}}
