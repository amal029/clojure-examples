(defproject first "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/core.typed "0.2.40"] 
                 [org.clojure/clojure "1.5.1"] [org.clojure/clojure-contrib "1.2.0"] [com.phansen/clojure.adt "1.0.0"] [org.clojure/core.match "0.2.1"]] 
  ;; :main ^:skip-aot first.evaluator
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
