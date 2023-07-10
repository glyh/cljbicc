(ns cljbicc.codegen
  (:require [clojure.core.match :as m]
            [cljbicc.asm :as asm]))

(defn panic [msg]
  (print msg)
  (flush)
  (System/exit 1))

(declare compile-exp)
(declare compile-assign)

(defn compile-binary [op lhs rhs] 
  (m/match 
    op
    :assign (compile-assign lhs rhs)
    _ (concat
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
        :ge [[:cmp :%rdi :%rax] [:setge :%al] [:movzb :%al :%rax]]
        _ (panic "Unexpected binary head\n")))))

; Compute the memory address of a given lval
(defn compile-addr [lval]
  (m/match lval
    [:id identifier] 
    (let [offset (* 8 (+ 1 (- (int identifier)) (int \a)))]
      [[:lea [:mem (- offset) :%rbp] :%rax]])
    _ (panic "Unexpected address\n")))

(defn compile-assign [lval rhs]
  (concat 
    (compile-addr lval)
    [[:push :%rax]] 
    (compile-exp rhs)
    [[:pop :%rdi]
     [:mov :%rax [:mem :%rdi]]]))

(defn compile-unary [op inner]
  (concat 
    (compile-exp inner)
    (m/match op
      :negative [[:neg :%rax]]
      _ (panic "Unexpected unary head\n"))))

(defn compile-exp [exp]
  (m/match exp
    [:id _] (concat (compile-addr exp) [[:mov [:mem :%rax] :%rax]])
    [op lhs rhs] (compile-binary op lhs rhs)
    [op inner] (compile-unary op inner)
    term [[:mov (asm/gas-term term) :%rax]]))

(defn compile-stmt
  "Compile a statement to assembly"
  [stmt]
  (m/match stmt
    [:expr-stmt [:exp expr]] (compile-exp expr)
    _ (panic "Unexpected stmt head\n")))
  
(defn compile-program 
  "Compile program ast to assembly"
  [ast]
  (m/match ast
    [:program & stmts]
    (asm/gas 
      [[:.globl :main]
       (concat
         [:main
          [:push :%rbp]
          [:mov :%rsp :%rbp]
          [:sub 208 :%rsp]] ; 26 local variables, and each take 8 byte
         (apply concat (map compile-stmt stmts))
         [[:mov :%rbp :%rsp]
          [:pop :%rbp]
          :ret])])
    _ (panic "Unexpected ast head\n")))

(defn codegen 
  [parse-result]
  (m/match parse-result
    [_ parsed] (compile-program parsed)
    {:line l :column c} (panic (format "Error on line %d col %d%n" l c))))
