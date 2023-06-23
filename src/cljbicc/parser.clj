(ns cljbicc.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def ignore (insta/parser (io/resource "ignore.bnf")))
(def cljbicc-parse
  (insta/parser 
    (io/resource "cljbicc.bnf")
    :auto-whitespace ignore))

(defn transform-exp 
  ([atom] atom)
  ([lhs op rhs] [(keyword op) lhs rhs]))

(def cljbicc-transforma
  (partial 
   insta/transform 
   {:exp transform-exp
    :INT (fn [i] (Integer/parseInt i))}))

(def parser (comp cljbicc-transforma cljbicc-parse))
