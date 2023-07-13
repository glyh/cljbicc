;; For now I am using metadata to store types. this is becaue I didn't consider that I need more information annotated to the AST. If redesigning this probably something else would be better.
;; And another part this code is ugly is that type checking and code generation is written together. Ideally there should be another phase solely for type checks.

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
    ;; we allow: addition between: 
    ;; num and num -> num
    ;; num and pointer -> pointer
    ;; pointer and num -> pointer
    :add 
    (let [rhs-compiled (compile-exp rhs)
          rhs-meta (meta rhs-compiled)
          lhs-compiled (compile-exp lhs)
          lhs-meta (meta lhs-compiled)]
      (m/match [(:type lhs-meta) (:type rhs-meta)]
        [(:or :int :any) (:or :int :any)]
        (with-meta
         (concat 
           rhs-compiled
           [[:push :%rax]]
           lhs-compiled
           [[:pop :%rdi]
            [:add :%rdi :%rax]])
         lhs-meta) 
        [[:pointer _] :int]
        (with-meta 
          (concat 
           rhs-compiled
           [[:shl 3 :%rax]
            [:push :%rax]]
           lhs-compiled
           [[:pop :%rdi]
            [:add :%rdi :%rax]])
          lhs-meta)
        [:int [:pointer _]]
        (with-meta 
          (concat 
           rhs-compiled
           [[:push :%rax]]
           lhs-compiled
           [[:shl 3 :%rax]
            [:pop :%rdi]
            [:add :%rdi :%rax]])
         rhs-meta)
        :else (panic "Type error: addition may only appear between ints or an int and a pointer, we got (%s, %s) for %s%n" lhs-meta rhs-meta [op lhs rhs])))
    ;; we allow: subtraction between: 
    ;; num and num -> num
    ;; pointer and num -> pointer
    :sub 
    (let [rhs-compiled (compile-exp rhs)
          rhs-meta (meta rhs-compiled)
          lhs-compiled (compile-exp lhs)
          lhs-meta (meta lhs-compiled)]
      (m/match [(:type lhs-meta) (:type rhs-meta)]
        [(:or :int :any) (:or :int :any)]
        (with-meta
          (concat
           rhs-compiled
           [[:push :%rax]]
           lhs-compiled
           [[:pop :%rdi]
            [:sub :%rdi :%rax]])
          lhs-meta)
        [[:pointer _] :int]
        (with-meta 
          (concat
           rhs-compiled
           [[:shl 3 :%rax]
            [:push :%rax]]
           lhs-compiled
           [[:pop :%rdi]
            [:sub :%rdi :%rax]])
          lhs-meta)

        :else (panic "Type error: subtraction may only appear between ints or a pointer and(before) an int, we got (%s, %s) for %s%n" lhs-meta rhs-meta [op lhs rhs])))
    _ 
    (let [rhs-compiled (compile-exp rhs)]
      (with-meta 
       (concat
        rhs-compiled
        [[:push :%rax]]
        (compile-exp lhs)
        [[:pop :%rdi]]
        (m/match op
         ; basic arithemetic
         :mul [[:imul :%rdi :%rax]]
         :div [:cqo [:idiv :%rdi]]
         ; comparison
         :eq [[:cmp :%rdi :%rax] [:sete :%al] [:movzb :%al :%rax]]
         :ne [[:cmp :%rdi :%rax] [:setne :%al] [:movzb :%al :%rax]]
         :lt [[:cmp :%rdi :%rax] [:setl :%al] [:movzb :%al :%rax]]
         :le [[:cmp :%rdi :%rax] [:setle :%al] [:movzb :%al :%rax]]
         :gt [[:cmp :%rdi :%rax] [:setg :%al] [:movzb :%al :%rax]]
         :ge [[:cmp :%rdi :%rax] [:setge :%al] [:movzb :%al :%rax]]
         _ (panic "Unexpected binary head\n")))
       (meta rhs-compiled)))))

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
  (let [rhs-compiled (compile-exp rhs)
        rhs-meta (meta rhs-compiled)]
    (with-meta
     (concat 
        (compile-addr lval)
        [[:push :%rax]] 
        rhs-compiled
        [[:pop :%rdi]
         [:mov :%rax [:mem :%rdi]]])
     rhs-meta))) 

(defn compile-unary [op inner]
  (m/match op
    :negative 
    (let [inner-compiled (compile-exp inner)
          inner-meta (meta inner-compiled)]
      (with-meta 
        (concat inner-compiled [[:neg :%rax]])
        inner-meta))
    :addr 
    (let [inner-compiled (compile-addr inner)
          inner-meta (meta inner-compiled)]
      (with-meta 
        inner-compiled
        (transform [:type] #(vector :pointer %1) inner-meta)))
      
    :deref 
    (let [inner-compiled (compile-exp inner)
          inner-meta (meta inner-compiled)]
      (with-meta 
        (concat inner-compiled [[:mov [:mem :%rax] :%rax]])
        (m/match (:type inner-meta)
           [:pointer pointed] (setval [:type] pointed inner-meta)
           :any inner-meta
           _ (panic "Type error: expected pointer in %s but got %s" inner (:type inner-meta)))))
      
    _ (panic "Unexpected unary head\n")))

(defn compile-exp [exp]
  (m/match exp
    [:id id] 
    (with-meta (concat (compile-addr [:id id]) [[:mov [:mem :%rax] :%rax]]) {:type :any})
    [op lhs rhs] (compile-binary op lhs rhs)
    [op inner] (compile-unary op inner)
    ;; NOTE: for now we only have integer and pointer so a term must be an integer
    term 
    ^{:type :int} [[:mov (asm/gas-term term) :%rax]]))

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
