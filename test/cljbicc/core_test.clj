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
