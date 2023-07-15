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

(def argregs [:%rdi :%rsi :%rdx :%rcx :%r8 :%r9])

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
        [:int :int]
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
        [:int :int]
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

(defn compile-addr [lval]
  (m/match lval
    [:id identifier] 
    (let [syminfo (select-one [ATOM :symtab identifier] info)]
      (if syminfo
        [[:lea [:mem (:offset syminfo) :%rbp] :%rax]]
        (panic "Unknown local variable %s" lval)))
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
           _ (panic "Type error: expected pointer in %s but got %s" inner (:type inner-meta)))))
      
    _ (panic "Unexpected unary head\n")))

(defn compile-exp [exp]
  (m/match exp
    ;; this won't work yet because for now we can't type check this.
    [:call [:id fn-name] & args] 
    (if (> (count args) 6)
      (panic "too many arguments for function %s at %s!" fn-name exp)
      (concat
        (mapcat #(concat (compile-exp %1) [:push :%rax]) args)
        (mapcat #(vector [:pop %1]) (reverse (take (count args) argregs)))
        [[:mov 0, :%rax]
         [:call fn-name]]))

    [:id id] 
    (with-meta (concat (compile-addr [:id id]) [[:mov [:mem :%rax] :%rax]]) 
               {:type (select-one [ATOM :symtab id :type] info)})
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


(defn get-type [cnt declspec]
  ; (printf "%s %s %n" cnt declspec)
  (if (= cnt 0)
    (keyword declspec)
    [:pointer (get-type (- cnt 1) declspec)]))

(defn compile-declaration
  [declspec [_ pointer-count name & maybe-exp]]
  (let [symtab (select-one [ATOM :symtab] info)]
    (if (symtab name)
      (panic "Redeclaration of variable %s in %s" name [type [pointer-count name maybe-exp]])
      (let [offset (* 8 (+ 1 (count symtab)))
            type (get-type pointer-count declspec)]
        (->>
         info
         (setval [ATOM :symtab name]
                 {:offset (- offset)
                  :type type})
         (setval [ATOM :stacksize] (align-to offset 16)))
        (if (empty? maybe-exp)
          []
          (compile-stmt [:expr-stmt [:exp [:assign [:id name] (first maybe-exp)]]]))))))
  
(defn compile-stmt
  "Compile a statement to assembly"
  [stmt]
  (m/match stmt
    [:if-stmt [:exp test] then else] (compile-if test then else)
    [:if-stmt [:exp test] then] (compile-if test then nop)
    [:for-stmt init test step body] (compile-for init test [:expr-stmt step] body)
    [:for-stmt init test body] (compile-for init test nop body)
    [:while-stmt test body] (compile-for nop [:expr-stmt test] nop body)

    [:declaration [:declspec type] & declarators] 
    (let [res (mapcat (partial compile-declaration type) declarators)]
      res)
    [:expr-stmt [:exp expr]] (compile-exp expr)
    [:expr-stmt] [] ; null statement

    [:block-stmt & stmts] (mapcat compile-stmt stmts)

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
