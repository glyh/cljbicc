(ns cljbicc.codegen
  (:require [clojure.core.match :as m]
            [cljbicc.asm :as asm])
  (:use     com.rpl.specter))

(defn panic [msg]
  (print msg)
  (flush)
  (System/exit 1))

(defn align-to [n align] (* align (quot (+ n align -1) align)))

(def info (atom nil))

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

(defn try-insert-local-var [{symtab :symtab :as info} identifier]
  (if (contains? symtab identifier)
    info
    (let [offset (* 8 (+ 1 (count symtab)))]
      (->> 
        info 
        (setval [:symtab identifier] (- offset))
        (setval [:stacksize] (align-to offset 16))))))
    
; Compute the memory address of a given lval
(defn query-or-generate-local-var-offset [identifier]
 (swap! info #(try-insert-local-var %1 identifier))
 (get-in @info [:symtab identifier]))

; (* 8 (+ 1 (- (int identifier)) (int \a)))
(defn compile-addr [lval]
  (m/match lval
    [:id identifier] 
    (let [offset (query-or-generate-local-var-offset identifier)]
      [[:lea [:mem offset :%rbp] :%rax]])
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
    [:id-atom id] (concat (compile-addr [:id id]) [[:mov [:mem :%rax] :%rax]])
    [op lhs rhs] (compile-binary op lhs rhs)
    [op inner] (compile-unary op inner)
    term [[:mov (asm/gas-term term) :%rax]]))

(defn generate-label-counter []
  (transform [ATOM :label-counter] inc info)
  (:label-counter @info))

(defn generate-if-label []
  (let [counter (generate-label-counter)]
    [(str ".L.else." counter)
     (str ".L.end" counter)]))
    
(declare compile-stmt)

(defn compile-if [params]
  (let [[else-label end-label] (generate-if-label)]
    (m/match params
      [[:exp test] then else]
      (concat
        (compile-exp test) 
        [[:cmp 0 :%rax] 
         [:je else-label]]
        (compile-stmt then)
        [[:jmp end-label]
         [:label else-label]]
        (compile-stmt else)
        [[:label end-label]])
      [[:exp test] then]
      (concat
        (compile-exp test) 
        [[:cmp 0 :%rax] 
         [:je end-label]]
        (compile-stmt then)
        [[:label end-label]])
      _ (panic "Syntax error for if statement\n"))))
      

(defn compile-stmt
  "Compile a statement to assembly"
  [stmt]
  (m/match stmt
    [:expr-stmt [:exp expr]] (compile-exp expr)
    [:if-stmt & params] (compile-if params)
    [:expr-stmt] [] ; null statement
    [:block-stmt & stmts] (apply concat (map compile-stmt stmts))
    [:return-stmt [:exp expr]] (concat (compile-exp expr) [[:jmp :.L.return]])
    _ (panic "Unexpected stmt head\n")))
  
(defn compile-program 
  "Compile program ast to assembly"
  [ast]
  (reset! info {:symtab {} :stacksize 0 :label-counter 0})
  (m/match ast
    [:program stmt]
    ;; Note that compile-stmt is effectful and it affects (:stacksize @info), we have to call it earlier
    (let [stmt-generated (compile-stmt stmt)]
      ; (println @info)
      (asm/gas 
        (concat
         [[:.globl :main]
          [:label "main"]
          [:push :%rbp]
          [:mov :%rsp :%rbp]
          [:sub (:stacksize @info) :%rsp]]
         stmt-generated
         [[:label ".L.return"]
          [:mov :%rbp :%rsp]
          [:pop :%rbp]
          :ret])))
    _ (panic "Unexpected ast head\n")))

(defn codegen 
  [parse-result]
  (m/match parse-result
    [_ parsed] (compile-program parsed)
    {:line l :column c} (panic (format "Error on line %d col %d%n" l c))))
