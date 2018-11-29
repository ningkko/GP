(defproject gp "0.1.0"
  :description "Lexicase in time series"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.csv "0.1.4"]
		 [clojure-csv/clojure-csv "2.0.1"]]
  :plugins [[org.clojars.benfb/lein-gorilla "0.5.3"]]
  :main ^:skip-aot ast.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
