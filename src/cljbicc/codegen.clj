(ns cljbicc.codegen
  (:require [clojure.core.match :as m]
            [cljbicc.asm :as asm]))
  
(declare compile-exp)

(defn compile-binary [op lhs rhs] 
  (concat
   (compile-exp rhs) 
   [[:push :%rax]]
   (compile-exp lhs)
   [[:pop :%rdi]]
   (m/match op
    ; basic arithemetic
    :add [[:add :%rdi :%rax]]
    :sub [[:sub :%rdi :%rax]]
    :mul [[:imul :%rdi :%rax]]
    :div [:cqo [:idiv :%rdi]]
    ; comparison
    :eq [[:cmp :%rdi :%rax] [:sete :%al] [:movzb :%al :%rax]]
    :ne [[:cmp :%rdi :%rax] [:setne :%al] [:movzb :%al :%rax]]
    :lt [[:cmp :%rdi :%rax] [:setl :%al] [:movzb :%al :%rax]]
    :le [[:cmp :%rdi :%rax] [:setle :%al] [:movzb :%al :%rax]]
    :gt [[:cmp :%rdi :%rax] [:setg :%al] [:movzb :%al :%rax]]
    :ge [[:cmp :%rdi :%rax] [:setge :%al] [:movzb :%al :%rax]])))

(defn compile-unary [op inner]
  (concat 
    (compile-exp inner)
    (m/match op
      :negative [[:neg :%rax]])))

(defn compile-exp [exp]
  (m/match exp
    [op lhs rhs] (compile-binary op lhs rhs)
    [op inner] (compile-unary op inner)
    term [[:mov (asm/gas-term term) :%rax]]))

(defn codegen
  "Compile code to a assembly"
  [code]
  (m/match code
    [_ parsed]
    (do 
     #_(printf "Parsed: %s%n" parsed)
     (asm/gas 
           [[:.global :main]
            (concat 
             [:main]
             (compile-exp parsed)
             [:ret])]))
    
    {:line l 
     :column c}
    (do 
      (printf "Error on line %d col %d%n" l c)
      (flush)
      (System/exit 1))))
