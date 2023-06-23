(ns cljbicc.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def ignore (insta/parser (io/resource "ignore.bnf")))
(def cljbicc-parse
  (insta/parser 
    (io/resource "cljbicc.bnf")
    :auto-whitespace ignore))

(def cljbicc-transform
  (partial 
   insta/transform 
   {:INT (fn [i] (Integer/parseInt i))}))

(def parser (comp cljbicc-transform cljbicc-parse))
