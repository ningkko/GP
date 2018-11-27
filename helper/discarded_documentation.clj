;; gorilla-repl.fileformat = 1

;; **
;;; # discarded
;;; 
;; **

;; @@
(ns discarded_documentation
  (:require [gorilla-plot.core :as plot]
   			[propel.core :refer :all]
   			[clojure-csv.core :refer :all]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;;Randomized multipoint crossover works the same as uniform crossover.
(comment
  (defn k-point-crossover-randomsized
    ""
    [point-number plushy-a plushy-b]
    (let [shorter (min-key count plushy-a plushy-b)
          longer (if (counted? plushy-a)
                   (max-key count plushy-b plushy-a)
                   (if (= shorter plushy-a)
                     plushy-b
                     plushy-a))
          target-length (count longer)
          new-gene-length (atom 0) ;; length of the new gene
          point-left (atom point-number)
          times (atom 0)
          length-diff (- target-length (count shorter))
          shorter-padded (concat shorter (repeat length-diff :crossover-padding))]

      (remove #(= % :crossover-padding)
              (concat
                (while 
                  (< new-gene-length target-length)
                  (do
                    (apply
                      #(let [chunk-size (+ 1 (rand-int (- (count %1) point-left 1 1)))]
                        (doall
                          (take chunk-size %1)
                          (drop chunk-size %1)
                          (drop chunk-size %2))
                        (if (even? times) 
                          [plushy-a
                           plushy-b]
                          [plushy-b
                           plushy-a]))) 

                    (swap! point-left dec)
                    (swap! times inc)))
                plushy-a
                plushy-b))))
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(comment
  "don' know how to return"
  (loop [x 0]
    (when 
      (<= x chunk-number)
      (drop x (take (inc x) a))
      (recur (inc x)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def instruction ['plus 'minus 'integer_+])
(def instruction2 ['plus 'integer_+ 'minus])

(let [length (count instruction) 
      mid (int (/ length 2))]
  (if (even? length)
    (concat (take mid instruction)
            (take-last mid instruction2))
    (concat (take mid instruction)
            (take-last (+ mid 1) instruction2))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>plus</span>","value":"plus"},{"type":"html","content":"<span class='clj-symbol'>integer_+</span>","value":"integer_+"},{"type":"html","content":"<span class='clj-symbol'>minus</span>","value":"minus"}],"value":"(plus integer_+ minus)"}
;; <=

;; @@
;;no need of binding data to global variable
;;also def is a macro and what should be at %1 is symbol but not string
;; bind input data name to data
(comment
  (doall;; forces lazysequences to be evaluated
  (map 
  	#(def %1 
  	   (vec (get-target-data "src/training_set_metadata.csv" %2)))
  	data-names
  	(range 12))))

(comment
;; v2
(doseq [n data-names number (range 12)] 
  (def n 
    (vec (get-target-data "src/training_set_metadata.csv" number))))

;;brutal-force biding
(def ra (vec (get-target-data "src/training_set_metadata.csv" 1)))
(def decl (vec (get-target-data "src/training_set_metadata.csv" 2)))
(def gal_l (vec (get-target-data "src/training_set_metadata.csv" 3)))
(def gal_b (vec (get-target-data "src/training_set_metadata.csv" 4)))
(def ddf (vec (get-target-data "src/training_set_metadata.csv" 5)))
(def hostgal_specz (vec (get-target-data "src/training_set_metadata.csv" 6)))
(def hostgal_photoz (vec (get-target-data "src/training_set_metadata.csv" 7)))
(def hostgal_photoz_err (vec (get-target-data "src/training_set_metadata.csv" 8)))
(def distmod (vec (get-target-data "src/training_set_metadata.csv" 9)))
(def mwebv (vec (get-target-data "src/training_set_metadata.csv" 10)))


)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(map #(float %) (range -10 11))

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>-10.0</span>","value":"-10.0"},{"type":"html","content":"<span class='clj-unkown'>-9.0</span>","value":"-9.0"},{"type":"html","content":"<span class='clj-unkown'>-8.0</span>","value":"-8.0"},{"type":"html","content":"<span class='clj-unkown'>-7.0</span>","value":"-7.0"},{"type":"html","content":"<span class='clj-unkown'>-6.0</span>","value":"-6.0"},{"type":"html","content":"<span class='clj-unkown'>-5.0</span>","value":"-5.0"},{"type":"html","content":"<span class='clj-unkown'>-4.0</span>","value":"-4.0"},{"type":"html","content":"<span class='clj-unkown'>-3.0</span>","value":"-3.0"},{"type":"html","content":"<span class='clj-unkown'>-2.0</span>","value":"-2.0"},{"type":"html","content":"<span class='clj-unkown'>-1.0</span>","value":"-1.0"},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"},{"type":"html","content":"<span class='clj-unkown'>1.0</span>","value":"1.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>7.0</span>","value":"7.0"},{"type":"html","content":"<span class='clj-unkown'>8.0</span>","value":"8.0"},{"type":"html","content":"<span class='clj-unkown'>9.0</span>","value":"9.0"},{"type":"html","content":"<span class='clj-unkown'>10.0</span>","value":"10.0"}],"value":"(-10.0 -9.0 -8.0 -7.0 -6.0 -5.0 -4.0 -3.0 -2.0 -1.0 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)"}
;; <=

;; @@
(comment
  (defn lexicase-selection
  [pop cases]
  (loop [survivors pop
         cases-left cases
         current-case (first (shuffle cases-left))]
    (if (or (empty? cases-left)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (loop [individual (first survivors)
             best-error Float/MAX_VALUE
             next-best-error (regression-error-function current-case individual)]
        (if (> best-error next-best-error)
          (recur individual individual)
          best-error next-best-error
          next-best-error (regression-error-function current-case individual)
          (recur individual (first (rest survivors))
                 best-error best-error
                 next-best-error (regression-error-function current-case individual)))))
    (recur survivors 
           cases-left (filter #(not (= current-case %)) cases-left)
           current-case (first (shuffle case-lefft)))))
          

)
;; first (shuffle (filter #(not (= current-case %)) cases-left))))



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
