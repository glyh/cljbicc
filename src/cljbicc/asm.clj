(ns cljbicc.asm
  (:require 
    [clojure.string :as string]
    [clojure.core.match :refer [match]]))

(defn gas-term [term]
  (cond 
    (keyword? term) (name term)
    (number? term) (str "$" term)
    :else (str term)))

(defn- gas-3ac [statement]
  (match statement
    [op lhs rhs] (format "  %s %s, %s" (gas-term op) (gas-term lhs) (gas-term rhs))
    [op reg] (format "  %s %s" (gas-term op) (gas-term reg))
    op           (str "  " (gas-term op))))

(defn- gas-section [[head & rest :as sect]]
  (if (string/starts-with? (name head) ".")
    (->>
      sect
      (map gas-term)
      (string/join " ")
      (str "  "))
    (->>
      rest
      (map gas-3ac)
      (string/join "\n")
      (str (name head) ":\n"))))

(defn gas [asm-tree]
  (->> 
    asm-tree
    (map gas-section)
    (string/join "\n")))
