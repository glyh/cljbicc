(ns cljbicc.parse
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.core.match :as m])
  (:use     com.rpl.specter))

(def ignore (insta/parser (io/resource "ignore.bnf")))
(def cljbicc-parse
  (insta/parser 
    (io/resource "cljbicc.bnf")
    :auto-whitespace ignore))

(defn parse-declarator [& params]
  ; (println params)
  (flush)
  (loop [params params
         stars  0]
    (m/match params
      (["*" & rest] :seq) (recur rest (+ 1 stars))
      ([[:id id] [:exp exp]] :seq) 
      [:declarator stars id exp]
      ([[:id id]] :seq) 
      [:declarator stars id])))

(def cljbicc-transform
  (partial 
   insta/transform 
   {:INT   (fn [i] (Integer/parseInt i))
    :IDENT identity ;; id only contains 1 char so it's safe to just grab the first character.
    :declarator parse-declarator}))

(def parse (comp cljbicc-transform cljbicc-parse))
