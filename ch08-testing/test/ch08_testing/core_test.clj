(ns ch08-testing.core-test
  (:require [clojure.test :refer :all]
            [ch08-testing.core :refer :all]))

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
               (doall (range "boom")))))

#_(deftest test-range-exception-3
  (is (thrown-with-msg? ClassCastException
                        #"java.lang.String cannot be cast to java.lang.Number"
                        (doall (range "boom")))))

