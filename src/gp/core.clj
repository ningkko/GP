(ns gp.core
  (:gen-class))

(defn -main
  "'lein run gp.ast'."
  [& args]
  (require (symbol (first args))))
