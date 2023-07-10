(ns cljbicc.parse
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
   {:INT (fn [i] (Integer/parseInt i))
    :IDENT (fn [id] (first id))})) ;; id only contains 1 char so it's safe to just grab the first character.

(def parse (comp cljbicc-transform cljbicc-parse))
