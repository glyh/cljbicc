(ns cljbicc.asm
  (:require 
    [clojure.string :as string]
    [clojure.core.match :refer [match]]))

(defn gas-term [term]
  (cond 
    (keyword? term) (name term)
    (number? term) (str "$" term)
    :else
    (match term
      [:mem reg] (str "(" (name reg) ")")
      [:mem offset reg] (str offset "(" (name reg) ")")
      _ (str term))))

(defn- gas-3ac [statement]
  (match statement
    [op lhs rhs] (format "  %s %s, %s" (gas-term op) (gas-term lhs) (gas-term rhs))
    [op reg] (format "  %s %s" (gas-term op) (gas-term reg))
    op           (str "  " (gas-term op))))

(defn- gas-section [[head & rest :as sect]]
  (if (#{:.globl} head)
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
    (map (comp #(str % "\n") gas-section))
    string/join))
