(ns gp.core
  (:gen-class))

(defn -main
  "'lein run gp.abs'."
  [& args]
  (require (symbol (first args))))
