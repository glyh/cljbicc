(ns cljbicc.core
  (:require [clojure.java.shell :as shell]
            [babashka.cli :as cli]
            [clojure.core.match :as m]


            [cljbicc.asm :as asm]
            [cljbicc.parser :as p])

  (:import  [java.io File])
  (:gen-class))

(defn compile-exp [exp]
  (m/match exp
    [:+ lhs term] (conj (compile-exp lhs) [:add (asm/gas-term  term) :%rax])
    [:- lhs term] (conj (compile-exp lhs) [:sub (asm/gas-term term) :%rax])
    term [[:mov (asm/gas-term term) :%rax]]))

(defn compile-cljbicc
  "Compile code to a assembly"
  [code]
  (let [[_ parsed] (p/parser code)]
    (asm/gas 
      [[:.global :main]
       (concat 
        [:main]
        (compile-exp parsed)
        [:ret])])))

(compile-cljbicc "1 + 3 - 4")
(defn run-x86-64 [asm]
  (let [tmp-file (File/createTempFile "tmp" "")
        tmp-file-path (.getAbsolutePath tmp-file)]
    (shell/sh "cc" "-x" "assembler" "-static" "-o" tmp-file-path "-" :in asm)
    (let [{:keys [exit]} (shell/sh tmp-file-path)]
      (.delete tmp-file)
      exit)))

(defn compile-and-run [exp]
  (->>
    exp 
    compile-cljbicc
    run-x86-64))

(def schema
  {:coerce
   {:exp :str}})

(defn -main [& args]
  (if-let [{:keys [exp]} (cli/parse-opts args schema)]
    (println (compile-and-run exp))
    (println "Please input an expression via `--exp`"))
  (System/exit 0)) ; because we use `sh` now we hang, using this we prevent ourself from hanging  
