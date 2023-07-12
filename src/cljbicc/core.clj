(ns cljbicc.core
  (:require [clojure.java.shell :as shell]
            [clojure.tools.cli :as cli]
            ; [clojure.core.match :as m]

            ; [cljbicc.asm :as asm]
            [cljbicc.parse :as p]
            [cljbicc.codegen :as c]
            [clojure.string :as string])

  (:import  [java.io File])
  (:gen-class))

(defn run-x86-64 [asm]
  (if (nil? asm)
    "nothing"
    (let [tmp-file (File/createTempFile "tmp" "")
          tmp-file-path (.getAbsolutePath tmp-file)
          as-result (shell/sh "cc" "-x" "assembler" "-static" "-o" tmp-file-path "-" :in asm)]
      #_(printf "Assembly generated:%n%s%n%n" asm)
      #_(printf "Assembler result:%n%s%n%s%n" (:out as-result) (:err as-result))
      (let [{:keys [exit]} (shell/sh tmp-file-path)]
        (.delete tmp-file)
        exit))))

(defn cljbicc-compile [exp]
  (->>
    exp
    p/parse
    c/codegen))

(defn compile-and-run [exp]
  ; (printf "Code fetched `%s`%n" exp)
  (->>
    exp 
    cljbicc-compile
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
