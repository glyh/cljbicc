(ns cljbicc.core
  (:require [clojure.string :as string]
            [clojure.java.shell :as shell]
            [clojure.tools.cli :as cli]

            [cljbicc.asm :as asm])
  (:import  [java.io File]))

(defn compile-cljbicc
  "Compile a number to a program"
  [num]
  (asm/gas 
    [[:.global :main]
     [:main 
      [:mov num :%rax]
      :ret]]))

(defn run-x86-64 [asm]
  (let [tmp-file (File/createTempFile "tmp" "")
        tmp-file-path (.getAbsolutePath tmp-file)]
    (shell/sh "cc" "-x" "assembler" "-static" "-o" tmp-file-path "-" :in asm)
    (let [exitcode (:exit (shell/sh tmp-file-path))]
      (.delete tmp-file)
      exitcode)))

(defn compile-and-run [num]
  (->>
    num 
    compile-cljbicc
    run-x86-64))

(def cli-options
  [["-n" "--number NUM" "Number to compile"
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])

(defn -main [& args]
  (if-let [{{num :number} :options} (cli/parse-opts args cli-options)]
    (if (not= num nil)
      (->>
       num
       compile-and-run
       (printf "Got exit code %s.%n"))
      (throw (Exception. "Please provide a number via `-n`")))
    (throw (Exception. "Please provide a number via `-n`"))))
