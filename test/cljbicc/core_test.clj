(ns cljbicc.core-test
  (:require [clojure.test :refer :all]
            [cljbicc.core :refer :all]))

(deftest simple-number
  (testing "Compiling a number"
    (is (= 42 (compile-and-run 42)))
    (is (= 0 (compile-and-run 0)))))
