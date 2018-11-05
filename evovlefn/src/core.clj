(ns gp.core
  (:gen-class))

(defn -main
  "lein run gp.evolvefn"
  [& args]
  (require (symbol (first args))))
