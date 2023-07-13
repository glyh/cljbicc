(ns cljbicc.core-test
  (:require [clojure.test :refer :all]
            [cljbicc.core :refer :all]))

(deftest simple-number
  (testing "Compiling a number"
    (is (= 42 (compile-and-run "return 42;")))
    (is (= 0 (compile-and-run "return 0;")))))

(deftest sum-and-difference
  (testing "Compiling a sum/different expression"
    (is (= 8 (compile-and-run "return 1 + 3 - 5 + 9;")))
    (is (= 21 (compile-and-run "return 5 + 20 - 4;")))))

(deftest basic-arithmetic 
  (testing "Compiling basic arithematic expression"
    (is (= 15 (compile-and-run "return 5 * (9 - 6);")))
    (is (= 4 (compile-and-run "return (3 + 5) /2;")))
    (is (= 7 (compile-and-run "return 1 + (4-2) * 3;")))))

(deftest unary-positive-negative
  (testing "Compiling basic arithematic expression"
    (is (= 10 (compile-and-run "return -10 +20;")))
    (is (= 10 (compile-and-run "return - - 10;")))
    (is (= 10 (compile-and-run "return - - + 10;")))))

(deftest comparison-eq-ne
  (testing "Compiling EQ/NE expression"
    (is (= 0 (compile-and-run "return 0 == 1;")))
    (is (= 1 (compile-and-run "return 42 == 42;")))
    (is (= 1 (compile-and-run "return 0 != 1;")))
    (is (= 0 (compile-and-run "return 42 != 42;")))))

(deftest comparison-le-lt
  (testing "Compiling LE/LT expression"
    (is (= 1 (compile-and-run "return 0 <  1;")))
    (is (= 0 (compile-and-run "return 1 <  1;")))
    (is (= 0 (compile-and-run "return 2 <  1;")))
    (is (= 1 (compile-and-run "return 0 <= 1;")))
    (is (= 1 (compile-and-run "return 1 <= 1;")))
    (is (= 0 (compile-and-run "return 2 <= 1;")))))

(deftest comparison-ge-gt
 (testing "Compiling LE/LT expression"
   (is (= 1 (compile-and-run "return 1 >  0;")))
   (is (= 0 (compile-and-run "return 1 >  1;")))
   (is (= 0 (compile-and-run "return 1 >  2;")))
   (is (= 1 (compile-and-run "return 1 >= 0;")))
   (is (= 1 (compile-and-run "return 1 >= 1;")))
   (is (= 0 (compile-and-run "return 1 >= 2;")))))

(deftest single-character-variables
  (testing "Compiling single character variables"
    (is (= 3 (compile-and-run "{a = 3; return a;}")))
    (is (= 8 (compile-and-run "{a = 3; z = 5; return a + z;}")))
    (is (= 6 (compile-and-run "{a = b = 3; return a + b;}")))))

(deftest multi-character-variables 
  (testing "Test multi char variables"
    (is (= 3 (compile-and-run "{foo = 3; return foo;}")))
    (is (= 8 (compile-and-run "{foo123 = 3; bar = 5; return foo123 + bar;}")))
    (is (= 6 (compile-and-run "{a = b = 3; return a + b;}")))))

(deftest return
  (testing "Test return"
    (is (= 1 (compile-and-run "{return 1; 2; 3;}")))
    (is (= 2 (compile-and-run "{1; return 2; 3;}")))
    (is (= 3 (compile-and-run "{1; 2; return 3;}")))))

(deftest block-stmt
  (testing "Test block stmt"
    (is (= 3 (compile-and-run "{ {1; {2;} return 3;} }")))))

(deftest null-stmt
  (testing "Test null stmt"
    (is (= 5 (compile-and-run "{ ;;;; return 5; }")))))

(deftest if-stmt
  (testing "Test if stmt"
    (is (= 3 (compile-and-run "{ if (0) return 2; return 3; }")))
    (is (= 3 (compile-and-run "{ if (1-1) return 2; return 3; }")))
    (is (= 2 (compile-and-run "{ if (1) return 2; return 3; }")))
    (is (= 2 (compile-and-run "{ if (2-1) return 2; return 3; }")))
    (is (= 4 (compile-and-run "if (0) { 1; 2; return 3; } else { return 4; } ")))
    (is (= 3 (compile-and-run "if (1) { 1; 2; return 3; } else { return 4; } ")))))
  
(deftest for-stmt
  (testing "Test for stmt"
    (is (= 55 (compile-and-run "{ i=0; j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }")))
    (is (= 3 (compile-and-run "{for(;;) {return 3; } return 5;}")))))
