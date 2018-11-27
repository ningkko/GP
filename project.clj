;; gorilla-repl.fileformat = 1

;; @@
(defproject gp "0.0.3"
  :description "playing around"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.csv "0.1.4"]
		 [clojure-csv/clojure-csv "2.0.1"]]
  :plugins [[org.clojars.benfb/lein-gorilla "0.5.3"]]
  :main ^:skip-aot gp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

;; @@
