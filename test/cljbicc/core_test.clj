(ns cljbicc.core-test
  (:require [clojure.test :refer :all]
            [cljbicc.core :refer :all]))

(deftest simple-number
  (testing "Compiling a number"
    (is (= 42 (compile-and-run "42")))
    (is (= 0 (compile-and-run "0")))))

(deftest sum-and-difference
  (testing "Compiling a sum/different expression"
    (is (= 8 (compile-and-run "1 + 3 - 5 + 9")))
    (is (= 21 (compile-and-run "5 + 20 - 4")))))

(deftest basic-arithmetic 
  (testing "Compiling basic arithematic expression"
    (is (= 15 (compile-and-run "5 * (9 - 6)")))
    (is (= 4 (compile-and-run "(3 + 5) /2")))
    (is (= 7 (compile-and-run "1 + (4-2) *    3")))))

(deftest unary-positive-negative
  (testing "Compiling basic arithematic expression"
    (is (= 10 (compile-and-run "-10 +20")))
    (is (= 10 (compile-and-run "- - 10")))
    (is (= 10 (compile-and-run "- - + 10")))))
