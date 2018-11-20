;; gorilla-repl.fileformat = 1

;; **
;;; # ast
;;; 
;;; #### TODO
;;; 1. Readin data (Done)
;;; 2. Get input data & target data (Done)
;;; 3. crossover (Testing)
;;; 4. mutation (if error > C, flip bit?)
;;; 5. Error function
;;; 6. Simple GP
;;; 7. function to decide which crossover to use overtime
;;; 8. tournament->lexicase
;;; 
;;; 
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
(vector 1 2 3)
(vector '(1 2 3))
(vec '(1 2 3))
;;exception:(vec 1 2 3)
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
  #(def %1 
     (vec (get-target-data "src/training_set_metadata.csv" %2)))
  data-names
  (range 12))

(comment
;; v2
(doseq [n data-names number (range 12)] 
  (def n 
    (vec (get-target-data "src/training_set_metadata.csv" number)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
ra


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
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;;remove
(remove pos? [1 -2 2 -1 3 7 0])


;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>-2</span>","value":"-2"},{"type":"html","content":"<span class='clj-long'>-1</span>","value":"-1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(-2 -1 0)"}
;; <=

;; @@
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
;; low possibility
(defn multi-point-crossover-parallel-odd
  "Multi point crossover is a generalization of the one-point crossover wherein alternating segments are swapped to get new off-springs...
  take odd genomes, uniform sized
  a-1+b-1+a-3+b-3+...+a_left+b_left"
  
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (counted? plushy-a)
                 (max-key count plushy-b plushy-a)
                 (if (= shorter plushy-a)
                   plushy-b
                   plushy-a))
        length (count longer) ;;length of genes
        chunk-number (+ 1 (rand-int length))
        chunk-size (int (/ length chunk-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))
        segmented-a (map vec (partition-all chunk-size plushy-a))
        segmented-b (map vec (partition-all chunk-size plushy-b))
        index (vec (filter even? (range (count segmented-a))))]
    
     (remove #(= % :crossover-padding) 
             (mapcat 
               #(concat (nth segmented-a %) (nth segmented-b %)) 
               index)))) 
	
    
;;high P
(defn multi-point-crossover-parallel-interleaving
  "Multi point crossover is a generalization of the one-point crossover wherein alternating segments are swapped to get new off-springs...
  take odd genomes, uniform sized
  a-1+b-2+a-3+b-4+...+a_left+b_left"
  
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (counted? plushy-a)
                 (max-key count plushy-b plushy-a)
                 (if (= shorter plushy-a)
                   plushy-b
                   plushy-a))
        length (count longer) ;;length of genes
        ;;at least 2 chunks'
        chunk-number (+ 2 (rand-int (dec length)))
        chunk-size (int (/ length chunk-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))
        segmented-a (map vec (partition-all chunk-size plushy-a))
        segmented-b (map vec (partition-all chunk-size plushy-b))
        index (range (count segmented-a))]
    
     (remove #(= % :crossover-padding) 
             (mapcat 
               #(nth 
                  (if (even? %)
                    segmented-a
                    segmented-b)
                  %)
               index)))) 
	
    
    
;; low probability
(defn multi-point-crossover-parallel-even
  "Multi point crossover is a generalization of the one-point crossover wherein alternating segments are swapped to get new off-springs...
  take odd genomes, uniform sized
  a-2+b-2+a-4+b-4+...+a_left+b_left"
  
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (counted? plushy-a)
                 (max-key count plushy-b plushy-a)
                 (if (= shorter plushy-a)
                   plushy-b
                   plushy-a))
        length (count longer) ;;length of genes
        ;;at least 2 chunks'
        chunk-number (+ 2 (rand-int (dec length)))
        chunk-size (int (/ length chunk-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))
        segmented-a (map vec (partition-all chunk-size plushy-a))
        segmented-b (map vec (partition-all chunk-size plushy-b))
        index (vec (filter odd? (range (count segmented-a))))]
    
     (remove #(= % :crossover-padding) 
             (mapcat 
               #(concat (nth segmented-a %) (nth segmented-b %)) 
               index)))) 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/multi-point-crossover-parallel-even</span>","value":"#'gp.propel-ast/multi-point-crossover-parallel-even"}
;; <=

;; @@
;;didn't work, a momento. And randomized multipoint crossover works the same as uniform crossover.
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
;;apply
(apply #(inc %) [1])

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; @@
;;min-key
;;min-key apply a function and return the one with least value
(min-key abs -8 3 4 5)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}
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
(def a (atom 1))
(type a)
(type @a)
(swap! a inc)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; @@
;; while
(def a (atom 10))                                
(while 
  (pos? @a) 
  (do 
    (println @a) 
    (swap! a dec)))


;; @@
;; ->
;;; 10
;;; 9
;;; 8
;;; 7
;;; 6
;;; 5
;;; 4
;;; 3
;;; 2
;;; 1
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;;dotimes
(defn Example []
   (dotimes [n 5]
   (println n)))
(Example)
;; @@
;; ->
;;; 0
;;; 1
;;; 2
;;; 3
;;; 4
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;;loop

(defn Example []
  (loop [x 10]
    (when (> x 1)
      (println x)
      (recur (- x 2))))) 
(Example)

;; @@
;; ->
;;; 10
;;; 8
;;; 6
;;; 4
;;; 2
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;;doseq

(defn Example []
  (doseq [n [0 1 2]]
    (println n)))
(Example)


;; @@
;; ->
;;; 0
;;; 1
;;; 2
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;;while
(defn Example []
   (def x (atom 1)) ;;atom, changable variable
   (while ( < @x 5 ) ;;@gets its value
      (do
         (println @x)
         (swap! x inc)))) ;; swap changes its value
(Example)

;; @@
;; ->
;;; 1
;;; 2
;;; 3
;;; 4
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; if do
;;"do multi-task under each condition"
(defn Example [] (
                   if (= 2 2)
                   (do(println "Both the values are equal")
                     (println "true"))
                   (do(println "Both the values are not equal")
                     (println "false"))))
(Example)


;; @@
;; ->
;;; Both the values are equal
;;; true
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;;case
(defn Example []
   (def x 5) 
   (case x 
     10 (println "x is 10")
     2 (println "x is 2")
     odd? (println "x is odd") ;; only case numbers, will not print this line
     5 (println "x is 5")
     (println "x is neither 5 nor 10")))

(Example)

;; @@
;; ->
;;; x is 5
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(comment
"Bit string mutation
The mutation of bit strings ensue through bit flips at random positions.

This mutation operator takes the chosen genome and inverts the bits (i.e. if the genome bit is 1, it is changed to 0 and vice versa).

Non-Uniform
The probability that amount of mutation will go to 0 with the next generation is increased by using non-uniform mutation operator. It keeps the population from stagnating in the early stages of the evolution. It tunes solution in later stages of evolution. This mutation operator can only be used for integer and float genes.

Uniform
This operator replaces the value of the chosen gene with a uniform random value selected between the user-specified upper and lower bounds for that gene. This mutation operator can only be used for integer and float genes.

Gaussian
This operator adds a unit Gaussian distributed random value to the chosen gene. If it falls outside of the user-specified lower or upper bounds for that gene, the new gene value is clipped. This mutation operator can only be used for integer and float genes.

Shrink
This operator adds a random number taken from a Gaussian distribution with mean equal to the original value of each decision variable characterizing the entry parent vector.")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;; a new gene pool
(def gene-pool 
  list )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/gene-pool</span>","value":"#'gp.propel-ast/gene-pool"}
;; <=

;; @@
(defn bit-mutation
  "see definition above.bMutation rate [0 1)"
  [plushy mutation-rate]
  (map #(if (<= (rand) mutation-rate)
             (rand-nth instructions)
             %) 
       plushy))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/bit-mutation</span>","value":"#'gp.propel-ast/bit-mutation"}
;; <=
