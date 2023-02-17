(ns ch08-testing.core-test
  (:require [clojure.test :refer :all]
            [ch08-testing.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest test-range-group
  (testing "Testing range"
   (is (= '(0 1 2 3 4) (range 5))
       "got 0-indexed seq when only end specified")
   (is (= '() (range 0)) "got empty seq when end index = 0")
   (is (= '(0 1) (range 2)))))

(deftest test-range-are
  (testing "Testing range with are"
    (are [expected endIndex]
        (= expected (range endIndex))
      '(0 1 2 3 4) 5
      '() 0)))

(deftest test-range-exception
  (try
    ;; doall for forcing seq realization
    (doall (range "boom"))
    (is nil)
    (catch ClassCastException e
      (is true))
    (catch Throwable t
      (is nil))))

(deftest test-range-exception-2
  (is (thrown? ClassCastException
               (doall (range "boom"))))

 #_(deftest test-range-exception-3
     (is (thrown-with-msg? ClassCastException
                           #"java.lang.String cannot be cast to java.lang.Number"
                           (doall (range "boom"))))))

;;; property based testing
(def range-count-eq-n
  (prop/for-all [n gen/int]
                (= n (count (range n)))))

(tc/quick-check 100 range-count-eq-n)
;; => {:shrunk {:total-nodes-visited 5, :depth 2, :pass? false, :result false, :result-data nil, :time-shrinking-ms 1, :smallest [-1]}, :failed-after-ms 3, :num-tests 4, :seed 1676615220179, :fail [-3], :result false, :result-data nil, :failing-size 3, :pass? false}

(def range-count-eq-n-2
  (prop/for-all [n gen/pos-int]
                (= n (count (range n)))))

(tc/quick-check 100 range-count-eq-n-2)
;; => {:result true, :pass? true, :num-tests 100, :time-elapsed-ms 4, :seed 1676615365695}

(defrecord Ingredient
    [name quantity unit])

(defmulti convert
  "convert quantity from unit1 to unit2. matching on [unit1 unit2]"
  (fn [unit1 unit2 quantity] [unit1 unit2]))

;; lb to oz
(defmethod convert [:lb :oz] [_ _ lb] (* lb 16))

;; oz to lb
(defmethod convert [:oz :lb] [_ _ oz] (/ oz 16))

(defmethod convert :default [u1 u2 q]
  (if (= u1 u2)
    q
    (assert false (str "unknown unit conversion from " u1 " to " u2))))

(defn ingredient+
  [{q1 :quantity u1 :unit :as i1}
   {q2 :quantity u2 :unit}]
  (assoc i1 :quantity (+ q1 (convert u2 u1 q2))))
(def gen-food
  (gen/elements ["flour" "sugar" "butter"]))

(def gen-unit
  (gen/elements [:oz :lb]))

(def gen-ingredient
  (gen/fmap map->Ingredient
            (gen/hash-map
             :name gen-food
             :quantity gen/s-pos-int
             :unit gen-unit)))

(gen/sample gen-ingredient)
;; => (#ch08_testing.core_test.Ingredient{:name "flour", :quantity 1, :unit :lb} #ch08_testing.core_test.Ingredient{:name "sugar", :quantity 2, :unit :lb} #ch08_testing.core_test.Ingredient{:name "sugar", :quantity 2, :unit :lb} #ch08_testing.core_test.Ingredient{:name "butter", :quantity 1, :unit :oz} #ch08_testing.core_test.Ingredient{:name "butter", :quantity 2, :unit :lb} #ch08_testing.core_test.Ingredient{:name "butter", :quantity 1, :unit :oz} #ch08_testing.core_test.Ingredient{:name "flour", :quantity 2, :unit :oz} #ch08_testing.core_test.Ingredient{:name "butter", :quantity 3, :unit :oz} #ch08_testing.core_test.Ingredient{:name "sugar", :quantity 6, :unit :lb} #ch08_testing.core_test.Ingredient{:name "flour", :quantity 4, :unit :lb})

(def identity-conversion-prop
  (prop/for-all [u gen-unit
                 n gen/s-pos-int]
                (= n (convert u u n))))

(def conversion-order-prop
  (prop/for-all [u1 gen-unit
                 u2 gen-unit
                 u3 gen-unit
                 u4 gen-unit
                 n gen/s-pos-int]
                (= (->> n (convert u1 u2) (convert u2 u3) (convert u3 u4))
                   (->> n (convert u1 u3) (convert u3 u2) (convert u2 u4)))))

(tc/quick-check 100 identity-conversion-prop)
;; => {:result true, :pass? true, :num-tests 100, :time-elapsed-ms 5, :seed 1676616765375}

(def roundtrip-conversion-prop
  (prop/for-all [u1 gen-unit u2 gen-unit
                 q gen/s-pos-int]
                (and (= q
                        (convert u1 u2 (convert u2 u1 q))
                        (convert u2 u1 (convert u1 u2 q))))))

(defn add-and-convert [i1 i2 i3 output-unit]
  (let [{:keys [quantity unit]} (ingredient+ i1 (ingredient+ i2 i3))]
    (convert unit output-unit quantity)))

(def associative-ingredient+-prop
  (prop/for-all [i1 gen-ingredient
                 i2 gen-ingredient
                 i3 gen-ingredient]
                (= (add-and-convert i1 i2 i3 (:unit i1))
                   (add-and-convert i3 i1 i2 (:unit i1))
                   (add-and-convert i2 i1 i3 (:unit i1)))))
