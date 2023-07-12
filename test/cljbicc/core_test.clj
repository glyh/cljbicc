(ns cljbicc.core-test
  (:require [clojure.test :refer :all]
            [cljbicc.core :refer :all]))

(deftest simple-number
  (testing "Compiling a number"
    (is (= 42 (compile-and-run "42;")))
    (is (= 0 (compile-and-run "0;")))))

(deftest sum-and-difference
  (testing "Compiling a sum/different expression"
    (is (= 8 (compile-and-run "1 + 3 - 5 + 9;")))
    (is (= 21 (compile-and-run "5 + 20 - 4;")))))

(deftest basic-arithmetic 
  (testing "Compiling basic arithematic expression"
    (is (= 15 (compile-and-run "5 * (9 - 6);")))
    (is (= 4 (compile-and-run "(3 + 5) /2;")))
    (is (= 7 (compile-and-run "1 + (4-2) *    3;")))))

(deftest unary-positive-negative
  (testing "Compiling basic arithematic expression"
    (is (= 10 (compile-and-run "-10 +20;")))
    (is (= 10 (compile-and-run "- - 10;")))
    (is (= 10 (compile-and-run "- - + 10;")))))

(deftest comparison-eq-ne
  (testing "Compiling EQ/NE expression"
    (is (= 0 (compile-and-run "0 == 1;")))
    (is (= 1 (compile-and-run "42 == 42;")))
    (is (= 1 (compile-and-run "0 != 1;")))
    (is (= 0 (compile-and-run "42 != 42;")))))

(deftest comparison-le-lt
  (testing "Compiling LE/LT expression"
    (is (= 1 (compile-and-run "0 <  1;")))
    (is (= 0 (compile-and-run "1 <  1;")))
    (is (= 0 (compile-and-run "2 <  1;")))
    (is (= 1 (compile-and-run "0 <= 1;")))
    (is (= 1 (compile-and-run "1 <= 1;")))
    (is (= 0 (compile-and-run "2 <= 1;")))))

(deftest comparison-ge-gt
 (testing "Compiling LE/LT expression"
   (is (= 1 (compile-and-run "1 >  0;")))
   (is (= 0 (compile-and-run "1 >  1;")))
   (is (= 0 (compile-and-run "1 >  2;")))
   (is (= 1 (compile-and-run "1 >= 0;")))
   (is (= 1 (compile-and-run "1 >= 1;")))
   (is (= 0 (compile-and-run "1 >= 2;")))))

(deftest single-character-variables
  (testing "Compiling single character variables"
    (is (= 3 (compile-and-run "a = 3; a;")))
    (is (= 8 (compile-and-run "a = 3; z = 5; a + z;")))
    (is (= 6 (compile-and-run "a = b = 3; a + b;")))))

(deftest multi-character-variables 
  (testing "Test multi char variables"
    (is (= 3 (compile-and-run "foo = 3; foo;")))
    (is (= 8 (compile-and-run "foo123 = 3; bar = 5; foo123 + bar;")))
    (is (= 6 (compile-and-run "a = b = 3; a + b;")))))
