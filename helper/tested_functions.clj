;; gorilla-repl.fileformat = 1

;; **
;;; # asttest
;;; 1. tournament (+)
;;; 2. uniform-crossover (+)
;;; 3. multipoint crossover (-)
;;; 4. bit-mutation (+)
;;; 
;;; 
;;; 
;; **

;; @@
(ns tested-functions
  (:require [gorilla-plot.core :as plot]
   			[propel.core :refer :all]
   			[clojure-csv.core :refer :all]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def plushy-a '(string_+ string_- string_* string_% string_+ string_- string_* string_% test1 test2))
(def plushy-b '(Ineger_+ Integer_- Integer_* Integer_% Ineger_+ Integer_- Integer_* Integer_% test2 test1))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tested-functions/plushy-b</span>","value":"#'tested-functions/plushy-b"}
;; <=

;; @@
(def plushy-a [:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2])
(def plushy-b [:Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1])
(def length (count plushy-a))
(def chunk-number 4)
(def chunk-size (int (/ length chunk-number)))
(def a (map vec (partition-all chunk-size plushy-a)))
(def b (map vec (partition-all chunk-size plushy-b)))

  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tested-functions/b</span>","value":"#'tested-functions/b"}
;; <=

;; @@
(def index (vec (filter even? (range (count a)))))
(mapcat #(concat (nth a %) (nth b %)) index)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:string_+</span>","value":":string_+"},{"type":"html","content":"<span class='clj-keyword'>:string_-</span>","value":":string_-"},{"type":"html","content":"<span class='clj-keyword'>:Ineger_+</span>","value":":Ineger_+"},{"type":"html","content":"<span class='clj-keyword'>:Integer_-</span>","value":":Integer_-"},{"type":"html","content":"<span class='clj-keyword'>:string_+</span>","value":":string_+"},{"type":"html","content":"<span class='clj-keyword'>:string_-</span>","value":":string_-"},{"type":"html","content":"<span class='clj-keyword'>:Ineger_+</span>","value":":Ineger_+"},{"type":"html","content":"<span class='clj-keyword'>:Integer_-</span>","value":":Integer_-"},{"type":"html","content":"<span class='clj-keyword'>:test1</span>","value":":test1"},{"type":"html","content":"<span class='clj-keyword'>:test2</span>","value":":test2"},{"type":"html","content":"<span class='clj-keyword'>:test2</span>","value":":test2"},{"type":"html","content":"<span class='clj-keyword'>:test1</span>","value":":test1"}],"value":"(:string_+ :string_- :Ineger_+ :Integer_- :string_+ :string_- :Ineger_+ :Integer_- :test1 :test2 :test2 :test1)"}
;; <=

;; @@
(defn multi-point-crossover-parallel-odd
  "Multi point crossover is a generalization of the one-point crossover wherein alternating segments are swapped to get new off-springs...
  take odd genomes, uniform sized
  a-1+b-1+a-3+b-3+...+a_left+b_left"
  
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                   plushy-b
                   plushy-a)
        length (count longer) ;;length of genes
        ;; at least 2 chunks
        chunk-number (+ 2 (rand-int (dec length)))
        chunk-size (int (/ length chunk-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))
        segmented-a (map vec (partition-all chunk-size plushy-a))
        segmented-b (map vec (partition-all chunk-size plushy-b))
        index (vec (filter even? (range (count segmented-a))))]
    
     (remove #(= % :crossover-padding) 
             (mapcat 
               #(vector (
                  concat 
                  (nth segmented-a %) 
                  (nth segmented-b %))) 
             index)))) 

(dotimes [n 30]
 (println (multi-point-crossover-parallel-odd plushy-a plushy-b))
)
;; @@
;; ->
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_*) (:string_* :string_% :test1 :Integer_* :Integer_% :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn multi-point-crossover-parallel-interleaving
  "Multi point crossover is a generalization of the one-point crossover wherein alternating segments are swapped to get new off-springs...
  take odd genomes, uniform sized
  a-1+b-2+a-3+b-4+...+a_left+b_left"
  
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                   plushy-b
                   plushy-a)
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tested-functions/multi-point-crossover-parallel-interleaving</span>","value":"#'tested-functions/multi-point-crossover-parallel-interleaving"}
;; <=

;; @@
(dotimes [n 30]
 (println (multi-point-crossover-parallel-interleaving plushy-a plushy-b))
)
;; @@
;; ->
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :string_- :string_* :string_% :string_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :string_- :string_* :string_% :string_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :string_- :string_* :string_% :string_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;; low probability
(defn multi-point-crossover-parallel-even
  "Multi point crossover is a generalization of the one-point crossover wherein alternating segments are swapped to get new off-springs...
  take odd genomes, uniform sized
  a-2+b-2+a-4+b-4+...+a_left+b_left"
  
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                   plushy-b
                   plushy-a)
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tested-functions/multi-point-crossover-parallel-even</span>","value":"#'tested-functions/multi-point-crossover-parallel-even"}
;; <=

;; @@
(dotimes [n 30]
 (println (multi-point-crossover-parallel-even plushy-a plushy-b))
)
;; @@
;; ->
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;;mutation
;; @@

;; @@
(defn bit-mutation
  "see definition above. Mutation rate [0 1)"
  [plushy mutation-rate]
  (loop [p plushy
         result []]

    (if (empty? p)
      result
      (recur (rest p)
             (conj result 
                     (if (<= (rand) mutation-rate)
                       (rand-nth default-instructions)
                       (first p)))))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tested-functions/bit-mutation</span>","value":"#'tested-functions/bit-mutation"}
;; <=

;; @@
(dotimes [n 30]
  (println (count(bit-mutation (concat plushy-a plushy-b) 0.6))))
;; @@
;; ->
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 20
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn crossover-test
  "Multi point crossover is a generalization of the one-point crossover wherein alternating segments are swapped to get new off-springs...
  take odd genomes, uniform sized
  a-1+b-2+a-3+b-4+...+a_left+b_left"
  
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                   plushy-b
                   plushy-a)
        length (count longer) ;;length of genes
        ;;at least 2 chunks'
        chunk-number (+ 2 (rand-int (dec length)))
        chunk-size (int (/ length chunk-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))
        segmented-a (map vec (partition-all chunk-size plushy-a))
        segmented-b (map vec (partition-all chunk-size plushy-b))]  
    
    (loop [use-a (rand-nth [true false])
           a segmented-a
           b segmented-b
           result []]
        
      (if (empty? a)
        (remove #(= % :crossover-padding) result)
        (recur (not use-a)
               (rest a)
               (rest b)
               (concat result (if use-a
                              (first a) 
                              (first b)))))))) 


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tested-functions/crossover-test</span>","value":"#'tested-functions/crossover-test"}
;; <=

;; @@
(dotimes [n 30]
  (println (crossover-test plushy-a plushy-b)))
;; @@
;; ->
;;; (:Ineger_+ :string_- :Integer_* :string_% :Ineger_+ :string_- :Integer_* :string_% :test2 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:string_+ :string_- :string_* :string_% :string_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:Ineger_+ :Integer_- :string_* :string_% :Ineger_+ :Integer_- :string_* :string_% :test2 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :string_- :string_* :string_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :string_- :string_* :string_% :test1 :test2)
;;; (:Ineger_+ :string_- :Integer_* :string_% :Ineger_+ :string_- :Integer_* :string_% :test2 :test2)
;;; (:Ineger_+ :string_- :Integer_* :string_% :Ineger_+ :string_- :Integer_* :string_% :test2 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:Ineger_+ :string_- :Integer_* :string_% :Ineger_+ :string_- :Integer_* :string_% :test2 :test2)
;;; (:Ineger_+ :Integer_- :string_* :string_% :Ineger_+ :Integer_- :string_* :string_% :test2 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:Ineger_+ :string_- :Integer_* :string_% :Ineger_+ :string_- :Integer_* :string_% :test2 :test2)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:Ineger_+ :string_- :Integer_* :string_% :Ineger_+ :string_- :Integer_* :string_% :test2 :test2)
;;; (:Ineger_+ :Integer_- :string_* :string_% :Ineger_+ :Integer_- :string_* :string_% :test2 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:Ineger_+ :Integer_- :string_* :string_% :Ineger_+ :Integer_- :string_* :string_% :test2 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:Ineger_+ :string_- :Integer_* :string_% :Ineger_+ :string_- :Integer_* :string_% :test2 :test2)
;;; (:Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :string_- :string_* :string_% :test1 :test2)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn multi-point-crossover-parallel
  "a1-b1-a3-b3-... or a-2-b2-a4-b4-..."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                   plushy-b
                   plushy-a)
        length (count longer) ;;length of genes
        chunk-number (+ 1 (rand-int length))
        chunk-size (int (/ length chunk-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))
        segmented-a (map vec (partition-all chunk-size plushy-a))
        segmented-b (map vec (partition-all chunk-size plushy-b))]
    
    (loop [start-at-0th (rand-nth [true false])
           a (if start-at-0th
               segmented-a
               (rest segmented-a))
           b (if start-at-0th
               segmented-b
               (rest segmented-b))
           result []]
        
      (if (empty? a)
        (remove #(= % :crossover-padding) result)
        (recur start-at-0th
               (rest (rest a))
               (rest (rest b))
               (concat result (first a) (first b))))))) 


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tested-functions/multi-point-crossover-parallel</span>","value":"#'tested-functions/multi-point-crossover-parallel"}
;; <=

;; @@
(dotimes [n 30]
  (println (multi-point-crossover-parallel plushy-a plushy-b)))(dotimes [n 30]
  (multi-point-crossover-parallel plushy-a plushy-b))
;; @@
;; ->
;;; (:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_* :string_* :string_% :test1 :Integer_* :Integer_% :test2)
;;; ()
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_+ :string_- :Ineger_+ :Integer_- :string_+ :string_- :Ineger_+ :Integer_- :test1 :test2 :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2 :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2 :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_* :string_* :string_% :test1 :Integer_* :Integer_% :test2)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2 :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_* :string_* :string_% :test1 :Integer_* :Integer_% :test2)
;;; (:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_* :string_* :string_% :test1 :Integer_* :Integer_% :test2)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; ()
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(reduce + [1 2 3])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}
;; <=

;; @@

(defn standard-error
	[argmap individual]
    (let [program (push-from-plushy (:plushy individual))
            inputs input
            correct-outputs target
            outputs (map (fn [input]
                           (peek-stack
                            (interpret-program 
                             program
                             (assoc empty-push-state :input {:in1 input})
                             (:step-limit argmap))
                            :float))
                         inputs)

            errors (/ (reduce + (mapcat (fn [correct-output output]
                                           (if (= output :no-stack-item)
                                             1000000
                                             (Float.
                                              (abs (- (square correct-output) 
                                                     (square output))))))
                                         correct-outputs
                                         outputs))
                               (count outputs))]

        (assoc individual
               :behaviors outputs
               :errors errors
               :total-error (let [total-err (apply +' errors)]
                              (if (Double/isNaN total-err)
                                2000000
                                total-err)))))
  
;; @@
