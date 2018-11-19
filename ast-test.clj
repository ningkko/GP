;; gorilla-repl.fileformat = 1

;; **
;;; # ast-test
;;; 1. tournament (+)
;;; 2. uniform-crossover (+)
;;; 3. multipoint crossover
;;; 
;; **

;; @@
(ns gp.propel-ast
  (:require [gorilla-plot.core :as plot]
   			[propel.core :refer :all]
            [gp.propel-ast :refer :all]
   			[clojure-csv.core :refer :all]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn multi-point-crossover-interleave
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
        point-number (+ 1 (rand-int (- length 2))) ;;how many cut points
        chunk-size (int (/ length (inc point-number))) ;; size per chunk
        index (atom 0) ;; pointer
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    
     (remove #(= % :crossover-padding)
              (concat
                (while (< (+ chunk-size @index) length);;while no overflow
                  (do
                    ;;I think I have to apply this function to the arguments but apply didn't work..
                    #(do(
                       ;;take a chunk from the current parent gene
                       (drop @index (take (+ (chunk-size @index)) %))
                       ;;update index
                       (swap! (+ @index chunk-size))))
                    (if (even? (int (/ @index chunk-size)));;start from parent a
                      plushy-a
                      plushy-b)))
                (drop index plushy-a);; concat with what's left in plushy-a
                (drop index plushy-b)))));;and plushy-b

(multi-point-crossover-interleave plushy-a plushy-b)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/k-point-crossover-interleave</span>","value":"#'user/k-point-crossover-interleave"}
;; <=

;; @@
(def plushy-a [:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2])
(def plushy-b [:Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1])
(comment
  (def length (count plushy-a))
(def chunk-number 4)
(def chunk-size (int (/ length chunk-number)))
(def a (map vec (partition-all chunk-size plushy-a)))
(def b (map vec (partition-all chunk-size plushy-b)))
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
        longer (if (counted? plushy-a)
                 (max-key count plushy-b plushy-a)
                 (if (= shorter plushy-a)
                   plushy-b
                   plushy-a))
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
                  (nth segmented-b %)))) 
             index))) 

(dotimes [n 30]
 (println (multi-point-crossover-parallel plushy-a plushy-b))
)
;; @@
;; ->
;;; (:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2 :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :string_- :Ineger_+ :Integer_- :string_+ :string_- :Ineger_+ :Integer_- :test1 :test2 :test2 :test1)
;;; (:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_* :string_* :string_% :test1 :Integer_* :Integer_% :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2 :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :Ineger_+ :Integer_- :string_+ :string_- :Ineger_+ :Integer_- :test1 :test2 :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2 :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_* :string_* :string_% :test1 :Integer_* :Integer_% :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :Ineger_+ :Integer_- :string_+ :string_- :Ineger_+ :Integer_- :test1 :test2 :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_* :string_* :string_% :test1 :Integer_* :Integer_% :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :Ineger_+ :Integer_- :string_+ :string_- :Ineger_+ :Integer_- :test1 :test2 :test2 :test1)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; (:string_+ :string_- :string_* :string_% :string_+ :string_- :string_* :string_% :test1 :test2 :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+)
;;; (:string_+ :Ineger_+ :string_* :Integer_* :string_+ :Ineger_+ :string_* :Integer_* :test1 :test2)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/multi-point-crossover-parallel-even</span>","value":"#'user/multi-point-crossover-parallel-even"}
;; <=

;; @@
(dotimes [n 30]
 (println (multi-point-crossover-parallel-even plushy-a plushy-b))
)
;; @@
;; ->
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_% :string_+ :string_- :Integer_% :Ineger_+ :Integer_- :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_* :string_% :Integer_* :Integer_% :string_* :string_% :Integer_* :Integer_%)
;;; (:string_- :string_* :string_% :test1 :test2 :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_- :Integer_- :string_% :Integer_% :string_- :Integer_- :string_% :Integer_% :test2 :test1)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn multi-point-crossover-parallel-interleaving
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/multi-point-crossover-parallel-interleaving</span>","value":"#'user/multi-point-crossover-parallel-interleaving"}
;; <=

;; @@
(dotimes [n 30]
 (println (multi-point-crossover-parallel-interleaving plushy-a plushy-b))
)
;; @@
;; ->
;;; (:string_+ :string_- :string_* :string_% :string_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :string_% :string_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :string_- :string_* :string_% :string_+ :Integer_- :Integer_* :Integer_% :test2 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :string_* :Integer_% :Ineger_+ :Integer_- :string_* :string_% :test1 :test1)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; (:string_+ :string_- :Integer_* :Integer_% :string_+ :string_- :Integer_* :Integer_% :test1 :test2)
;;; (:string_+ :Integer_- :string_* :Integer_% :string_+ :Integer_- :string_* :Integer_% :test1 :test1)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
