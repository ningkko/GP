;; gorilla-repl.fileformat = 1

;; @@
(defproject gp "0.0.3"
  :description "playing around"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.4"]
		 [clojure-csv/clojure-csv "2.0.1"]]
  :plugins [[lein-idefiles "0.2.0"]
            [lein-gorilla "0.4.0"]]
  :main ^:skip-aot gp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

;; @@
