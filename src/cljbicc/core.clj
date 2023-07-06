(ns cljbicc.core
  (:require [clojure.java.shell :as shell]
            [clojure.tools.cli :as cli]
            [clojure.core.match :as m]

            [cljbicc.asm :as asm]
            [cljbicc.parser :as p]
            [clojure.string :as string])

  (:import  [java.io File])
  (:gen-class))

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

(defn compile-cljbicc
  "Compile code to a assembly"
  [code]
  (m/match (p/parser code)
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

; (compile-cljbicc "- + + - 10")

(defn run-x86-64 [asm]
  (if (nil? asm)
    "nothing"
    (let [tmp-file (File/createTempFile "tmp" "")
          tmp-file-path (.getAbsolutePath tmp-file)
          as-result (shell/sh "cc" "-x" "assembler" "-static" "-o" tmp-file-path "-" :in asm)]
      ; (printf "Assembly generated:%n%s%n%n" asm)
      ; (printf "Assembler result:%n%s%n%s%n" (:out as-result) (:err as-result))
      (let [{:keys [exit]} (shell/sh tmp-file-path)]
        (.delete tmp-file)
        exit))))

(defn compile-and-run [exp]
  (->>
    exp 
    compile-cljbicc
    run-x86-64))

(def cli-options
  [["-e" "--exp EXP" "Expression to compile"]
   ["-h" "--help"]])

(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond 
      (:exp options)
      (compile-and-run (:exp options))
      :else 
      (print "Error:\n  " (string/join "\n  " errors) "\n" "Usage:\n" summary))
    (flush) (System/exit 0))) 
