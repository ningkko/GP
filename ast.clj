;; gorilla-repl.fileformat = 1

;; **
;;; # ast
;;; 
;;; #### TODO
;;; 1. Readin data (Done)
;;; 2. Get input data & target data (Done)
;;; 2. crossover
;;; 2. Error function
;;; 3. Simple GP
;;; 4. function to decide which crossover to use overtime
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
(comment
  (vector 1 2 3)
  [1 2 3]
  (vector '(1 2 3))
  [(1 2 3)]
  (vec '(1 2 3))
  [1 2 3]
  (vec 1 2 3)
  ;;Exception thrown: clojure.lang.ArityException (Wrong number of args (3) passed to: core/vec)
)


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
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
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"},{"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/p1__10254#</span>","value":"#'gp.propel-ast/p1__10254#"}],"value":"(#'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254# #'gp.propel-ast/p1__10254#)"}
;; <=

;; @@
;;ra

;; @@

;; @@
;;brutal-force biding
(comment
  (def ra (vec (get-target-data "src/training_set_metadata.csv" 1)))
  (def decl (vec (get-target-data "src/training_set_metadata.csv" 2)))
  (def gal_l (vec (get-target-data "src/training_set_metadata.csv" 3)))
  (def gal_b (vec (get-target-data "src/training_set_metadata.csv" 4)))
  (def ddf (vec (get-target-data "src/training_set_metadata.csv" 5)))
  (def hostgal_specz (vec (get-target-data "src/training_set_metadata.csv" 6)))
  (def hostgal_photoz (vec (get-target-data "src/training_set_metadata.csv" 7)))
  (def hostgal_photoz_err (vec (get-target-data "src/training_set_metadata.csv" 8)))
  (def distmod (vec (get-target-data "src/training_set_metadata.csv" 9)))
  (def mwebv (vec (get-target-data "src/training_set_metadata.csv" 10))))
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/boolean_is-positive</span>","value":"#'gp.propel-ast/boolean_is-positive"}
;; <=

;; @@
(defn tournament-selection-revised
  "Elements are sorted according to their erropr first and then the first half will be taken. After which 1/10 of them will be selected"
  [pop]
  (let [half-size (/ (count pop) 2)
        tournament-set (take half-size (apply min-key :total-error pop))
        tournament-size (/ (count tournament-set) 10)]
        (take tournament-size (shuffle pop))
    ))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/tournament-selection-revised</span>","value":"#'gp.propel-ast/tournament-selection-revised"}
;; <=

;; @@
;;min-key
(comment
  min-key apply a function and return the one with least value
  (min-key abs -8 3 4 5)
  3)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn uniform-crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (counted? plushy-a);;choose a quicker way to set longer&shorter
                 (max-key count plushy-b plushy-a)
                 (if (= shorter plushy-a)
                   plushy-b
                   plushy-a))
        
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/uniform-crossover</span>","value":"#'gp.propel-ast/uniform-crossover"}
;; <=

;; @@
(defn k-point-crossover-odduni
  "k-point crossover is equivalent to performing k single-point crossovers with different crossover points..
  take odd genomes, uniform
  a-1+b-1+a-3+b-3+..."
  
  [point-number plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (counted? plushy-a)
                 (max-key count plushy-b plushy-a)
                 (if (= shorter plushy-a)
                   plushy-b
                   plushy-a))
        length (count longer)
        chunk-size (int (/ length point-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    
     (remove #(= % :crossover-padding)
              (concat
                (repeatedly 
                  (int (/ length chunk-size))
                  #((take chunk-size %1)
                    (drop (* chunk-size 2) %1)
                    (take chunk-size %2)
                    (drop (* chunk-size 2) %2))
                  plushy-a
                  plushy-b)
                plushy-a
                plushy-b))))
	
	
    
    
    
    
(defn k-point-crossover-interuni
  "k-point crossover is equivalent to performing k single-point crossovers with different crossover points..
  interleaving, uniform
  a-1+b-2+a-3+b-4+..."
  
  [point-number plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (counted? plushy-a)
                 (max-key count plushy-b plushy-a)
                 (if (= shorter plushy-a)
                   plushy-b
                   plushy-a))
        length (count longer)
        chunk-size (int (/ length point-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    
     (remove #(= % :crossover-padding)
              (concat
                (repeatedly 
                  (int (/ length chunk-size))
                  #((take chunk-size %1)
                    (drop (* chunk-size 2) %1)
                    (take chunk-size %2)
                    (drop (* chunk-size 2) %2))
                  (drop chunk-size plushy-a)
                  plushy-b)
                plushy-a
                plushy-b))))
	
    
    
    
    
(defn k-point-crossover-evenuni
  "k-point crossover is equivalent to performing k single-point crossovers with different crossover points..
  take odd genomes, uniform
  a-2+b-2+a-2+b-2+..."
  
  [point-number plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (counted? plushy-a)
                 (max-key count plushy-b plushy-a)
                 (if (= shorter plushy-a)
                   plushy-b
                   plushy-a))
        length (count longer)
        chunk-size (int (/ length point-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    
    (remove #(= % :crossover-padding)
              (concat
                (repeatedly 
                  (int (/ length chunk-size))
                  #((take chunk-size %1)
                    (drop (* chunk-size 2) %1)
                    (take chunk-size %2)
                    (drop (* chunk-size 2) %2))
                  (drop chunk-size plushy-a)
                  (drop chunk-size plushy-b))
                plushy-a
                plushy-b))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/k-point-crossover-evenuni</span>","value":"#'gp.propel-ast/k-point-crossover-evenuni"}
;; <=

;; @@
(defn k-point-crossover-random
  "k-point crossover is equivalent to performing k single-point crossovers with different crossover points.."
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/k-point-crossover-random</span>","value":"#'user/k-point-crossover-random"}
;; <=

;; @@
(apply #(inc %) [1])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; @@
;;remove
(comment
  (remove pos? [1 -2 2 -1 3 7 0])
  ;;(-2 -1 0)
  )

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/instruction</span>","value":"#'gp.propel-ast/instruction"}
;; <=

;; @@
(comment
  (def instruction ['plus 'minus 'integer_+])
  (type 
    (nth 
      (repeatedly ;;repeats functions
        (+ 1 (rand-int (count instruction))) 
        #(rand-nth instruction)) 0)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;; take first half of the first laziseq
;; and the second half of the second lazy seq
(comment
  (def instruction ['plus 'minus 'integer_+])
  (def instruction2 ['plus 'integer_+ 'minus])

  (let [length (count instruction) 
        mid (int (/ length 2))]
    (if (even? length)
      (concat (take mid instruction)
              (take-last mid instruction2))
      (concat (take mid instruction)
              (take-last (+ mid 1) instruction2)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;;atom, swap!, @
(comment 
  (def a (atom 1))
  (type a)
  (type @a)
  (swap! a inc)
  ;;#'gp.propel-ast/a
  ;;clojure.lang.Atom
  ;;java.lang.Long
  ;;2
 )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;; while
(comment
(def a (atom 10))                                
(while 
  (pos? @a) 
  (do 
    (println @a) 
    (swap! a dec)))

10
9
8
7
6
5
4
3
2
1)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;;dotimes
(comment
(defn Example []
   (dotimes [n 5]
   (println n)))
(Example)
0
1
2
3
4
)

;;loop
(comment
  (defn Example []
   (loop [x 10]
      (when (> x 1)
         (println x)
         (recur (- x 2))))) 
  (Example)
  10
  8
  6
  4
  2)

;;doseq
(comment
  (defn Example []
   (doseq [n [0 1 2]]
   (println n)))
(Example)
0
1
2)

;;while
(comment
(defn Example []
   (def x (atom 1)) ;;atom, changable variable
   (while ( < @x 5 ) ;;@gets its value
      (do
         (println @x)
         (swap! x inc)))) ;; swap changes its value
(Example))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;; if do
(comment
  "do multi-task under each condition"
  (defn Example [] (
   if (= 2 2)
      (do(println "Both the values are equal")
         (println "true"))
      (do(println "Both the values are not equal")
         (println "false"))))
(Example)
Both the values are equal
true
  )


;;case
(comment
(defn Example []
   (def x 5) 
   (case x 
     10 (println "x is 10")
     2 (println "x is 2")
     odd? (println "x is odd") ;; only case numbers, will not print this line
     5 (println "x is 5")
     (println "x is neither 5 nor 10")))

(Example)
x is 5
)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@
