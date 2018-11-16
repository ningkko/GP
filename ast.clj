;; gorilla-repl.fileformat = 1

;; **
;;; # ast
;;; 
;;; #### TODO
;;; 1. Readin data Done
;;; 2. Get input data & target data Done
;;; 2. Error function
;;; 3. Simple GP
;;; 4. lexicase
;;; 
;;; 
;; **

;; @@
(ns gp.propel-ast
  (:require [gorilla-plot.core :as plot]
   			[propel.core :refer :all]
   			[clojure-csv.core :refer :all]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn readin-data
  [file-name]
  (with-open [reader (io/reader file-name)]
  (doall
    (csv/read-csv reader))))
;c
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/readin-data</span>","value":"#'gp.propel-ast/readin-data"}
;; <=

;; @@
(defn read-column [filename column-index]
  (with-open [reader (io/reader filename)]
    (let [data (csv/read-csv reader)]
      (doall
        (map #(nth % column-index) data)))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/read-column</span>","value":"#'gp.propel-ast/read-column"}
;; <=

;; @@
(type (readin-data "src/training_set_metadata.csv"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-class'>clojure.lang.LazySeq</span>","value":"clojure.lang.LazySeq"}
;; <=

;; @@
;;(apply #(nth % 11) (readin-data "src/training_set_metadata.csv"))
;; @@

;; @@
;;( type 
;;	(read-column "src/training_set_metadata.csv" 11))

(defn get-target-data
  [file-name target-column]
  (rest 
    (apply vector 
           (read-column file-name target-column))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/get-target-data</span>","value":"#'gp.propel-ast/get-target-data"}
;; <=

;; @@

;; @@

;; @@

;; @@

;; @@
(vector 1 2 3)
;;[1 2 3]
(vector '(1 2 3))
;;[(1 2 3)]
(vec '(1 2 3))
;;[1 2 3
;;(vec 1 2 3)
;;Exception thrown: clojure.lang.ArityException (Wrong number of args (3) passed to: core/vec)


;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 2 3]"}
;; <=

;; @@
(def data-names ["object_id" "ra" "decl" "gal_l" "gal_b" "ddf" "hostgal_specz" "hostgal_photoz" "hostgal_photoz_err" "distmod" "mwebv" "target"])
(nth data-names 1)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;ra&quot;</span>","value":"\"ra\""}
;; <=

;; @@
;; bind input data name to data
(map 
  #(def %1 (vec 
             (get-target-data "src/training_set_metadata.csv" %2)))
  data-names
  (range 12))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10267#</span>","value":"#'gp.propel-ast/p1__10267#"}],"value":"(#'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267# #'gp.propel-ast/p1__10267#)"}
;; <=

;; @@
;;ra

;; @@

;; @@

;;(def ra (vec (get-target-data "src/training_set_metadata.csv" 1)))
;;(def decl (vec (get-target-data "src/training_set_metadata.csv" 2)))
;;(def gal_l (vec (get-target-data "src/training_set_metadata.csv" 3)))
;;(def gal_b (vec (get-target-data "src/training_set_metadata.csv" 4)))
;;(def ddf (vec (get-target-data "src/training_set_metadata.csv" 5)))
;;(def hostgal_specz (vec (get-target-data "src/training_set_metadata.csv" 6)))
;;(def hostgal_photoz (vec (get-target-data "src/training_set_metadata.csv" 7)))
;;(def hostgal_photoz_err (vec (get-target-data "src/training_set_metadata.csv" 8)))
;;(def distmod (vec (get-target-data "src/training_set_metadata.csv" 9)))
;;(def mwebv (vec (get-target-data "src/training_set_metadata.csv" 10)))
;; @@

;; @@
;; some supplements
(defn string_absolute
  [state]
  (make-push-instruction state
                         #(max % (- %))
                         [:integer]
                         :integer))

(defn boolean_is-negative
  [state]
  (make-push-instruction state
                         #(neg? %)
                         [:integer]
                         :boolean))


(defn boolean_is-positive
  [state]
  (make-push-instruction state
                         #(pos? %)
                         [:integer]
                         :boolean))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/string_absolute</span>","value":"#'gp.propel-ast/string_absolute"}
;; <=

;; @@

;; @@
