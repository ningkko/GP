(defproject evovlfn "0.1.0-SNAPSHOT"
  :description "Based on Lee Spector's evolvefn."
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot gorilla-test.core
  :target-path "target/%s" 
  :bikeshed {:max-line-length 60
             :var-redefs false
             :name-collisions false}
  :plugins [[lein-gorilla "0.4.0"]]
  :profiles {:uberjar {:aot :all}})
