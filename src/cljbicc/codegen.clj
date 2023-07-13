(ns cljbicc.codegen
  (:require [clojure.core.match :as m]
            [cljbicc.asm :as asm])
  (:use     com.rpl.specter))

(defn panic [& msg-param]
  (apply printf msg-param)
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

(defn compile-addr [lval]
  (m/match lval
    [:id identifier] 
    (let [offset (query-or-generate-local-var-offset identifier)]
      [[:lea [:mem offset :%rbp] :%rax]])
    [:deref inner]
    (compile-exp inner)
    _ (panic "Unexpected address: %s\n" lval)))

(defn compile-assign [lval rhs]
  (concat 
    (compile-addr lval)
    [[:push :%rax]] 
    (compile-exp rhs)
    [[:pop :%rdi]
     [:mov :%rax [:mem :%rdi]]]))

(defn compile-unary [op inner]
  (m/match op
    :negative (concat (compile-exp inner) [[:neg :%rax]])
    :addr (compile-addr inner)
    :deref (concat (compile-exp inner) [[:mov [:mem :%rax] :%rax]])
    _ (panic "Unexpected unary head\n")))

(defn compile-exp [exp]
  (m/match exp
    [:id id] (concat (compile-addr [:id id]) [[:mov [:mem :%rax] :%rax]])
    [op lhs rhs] (compile-binary op lhs rhs)
    [op inner] (compile-unary op inner)
    term [[:mov (asm/gas-term term) :%rax]]))

(defn generate-label-counter []
  (transform [ATOM :label-counter] inc info)
  (:label-counter @info))

(defn generate-if-label []
  (let [counter (generate-label-counter)]
    [(str ".L.else." counter)
     (str ".L.end_if." counter)]))
    
(declare compile-stmt)

(defn compile-if [test then else]
  (let [[else-label end-label] (generate-if-label)]
    (concat
     (compile-exp test) 
     [[:cmp 0 :%rax] 
      [:je else-label]]
     (compile-stmt then)
     [[:jmp end-label]
      [:label else-label]]
     (compile-stmt else)
     [[:label end-label]])))

(defn generate-for-label []
  (let [counter (generate-label-counter)]
    [(str ".L.begin_for." counter)
     (str ".L.end_for." counter)]))

(def nop [:expr-stmt])

(defn compile-for [init test step body]
  (let [[begin-label end-label] (generate-for-label)]
    (concat 
     (compile-stmt init)
     [[:label begin-label]]
     (if (not= nop test) 
       (concat 
        (compile-stmt test)
        [[:cmp 0 :%rax]
         [:je end-label]])
       [])
     (compile-stmt body)
     (compile-stmt step)
     [[:jmp begin-label]
      [:label end-label]])))
  
(defn compile-stmt
  "Compile a statement to assembly"
  [stmt]
  (m/match stmt
    [:expr-stmt [:exp expr]] (compile-exp expr)
    [:if-stmt [:exp test] then else] (compile-if test then else)
    [:if-stmt [:exp test] then] (compile-if test then nop)
    [:for-stmt init test step body] (compile-for init test [:expr-stmt step] body)
    [:for-stmt init test body] (compile-for init test nop body)
    [:while-stmt test body] (compile-for nop [:expr-stmt test] nop body)
    [:expr-stmt] [] ; null statement
    [:block-stmt & stmts] (apply concat (map compile-stmt stmts))
    [:return-stmt [:exp expr]] (concat (compile-exp expr) [[:jmp :.L.return]])
    _ (panic "Unexpected stmt head: %s\n" stmt)))
  
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
    _ (panic "Unexpected ast head: %s\n" ast)))

(defn codegen 
  [parse-result]
  (m/match parse-result
    [:S parsed] (compile-program parsed)
    {:line l :column c} (panic "Error on line %d col %d%n" l c)))
