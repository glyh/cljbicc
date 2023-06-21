(ns cljbicc.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.tools.cli :refer [parse-opts]]))

(defn long-str [& strings] (clojure.string/join "\n" strings))

(defn compile-cljbicc
  "Compile a number to a program"
  [num]
  (long-str
   "  .globl main"
   "main:"
   (format "  mov $%d, %%rax" num)
   "  ret"))

(defn run-x86-64 [asm]
  (let [tmp-file (java.io.File/createTempFile "tmp" "")
        tmp-file-path (.getAbsolutePath tmp-file)]
    (sh "cc" "-x" "assembler" "-static" "-o" tmp-file-path "-" :in asm)
    (let [exitcode (:exit (sh tmp-file-path))]
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
  (if-let [{{num :number} :options} (parse-opts args cli-options)]
    (if (not= num nil)
      (->>
       num
       compile-cljbicc
       run-x86-64
       (printf "Got exit code %s.%n"))
      (throw (Exception. "Please provide a number via `-n`")))
    (throw (Exception. "Please provide a number via `-n`"))))
