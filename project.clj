(defproject cljbicc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.12"]
                 [org.clojure/core.match "1.0.1"]
                 [org.babashka/cli "0.7.52"]]
  :repl-options {:init-ns cljbicc.core}
  :main cljbicc.core
  :resource-paths ["res"]
  :profiles {:uberjar {:aot :all}})
