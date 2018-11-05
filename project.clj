(defproject gp "0.0.3"
  :description "Rvised from Lee Spector's GP tutorial"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :plugins [[lein-idefiles "0.2.0"]
            [lein-gorilla "0.4.0"]]
  :main ^:skip-aot gp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})