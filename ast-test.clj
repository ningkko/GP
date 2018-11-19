;; gorilla-repl.fileformat = 1

;; **
;;; # ast-test
;;; 1. tournament (+)
;;; 2. uniform-crossover (+)
;;; 3. multipoint crossover (-)
;;; 4. bit-mutation (+)
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
(def plushy-a '(string_+ string_- string_* string_% string_+ string_- string_* string_% test1 test2))
(def plushy-b '(Ineger_+ Integer_- Integer_* Integer_% Ineger_+ Integer_- Integer_* Integer_% test2 test1))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/plushy-b</span>","value":"#'gp.propel-ast/plushy-b"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/b</span>","value":"#'gp.propel-ast/b"}
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
                  (nth segmented-b %))) 
             index)))) 

(dotimes [n 30]
 (println (multi-point-crossover-parallel-odd plushy-a plushy-b))
)
;; @@
;; ->
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_*) (:string_* :string_% :test1 :Integer_* :Integer_% :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_*) (:string_* :string_% :test1 :Integer_* :Integer_% :test2))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :string_* :Ineger_+ :Integer_- :Integer_*) (:string_* :string_% :test1 :Integer_* :Integer_% :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :string_- :Ineger_+ :Integer_-) (:string_+ :string_- :Ineger_+ :Integer_-) (:test1 :test2 :test2 :test1))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :Ineger_+) (:string_* :Integer_*) (:string_+ :Ineger_+) (:string_* :Integer_*) (:test1 :test2))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
;;; ((:string_+ :string_- :string_* :string_% :string_+ :Ineger_+ :Integer_- :Integer_* :Integer_% :Ineger_+))
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/multi-point-crossover-parallel-interleaving</span>","value":"#'gp.propel-ast/multi-point-crossover-parallel-interleaving"}
;; <=

;; @@
(dotimes [n 30]
 (println (multi-point-crossover-parallel-interleaving plushy-a plushy-b))
)
;; @@
;; ->
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ string_- Integer_* Integer_% string_+ string_- Integer_* Integer_% test1 test2)
;;; (string_+ string_- Integer_* Integer_% string_+ string_- Integer_* Integer_% test1 test2)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ string_- Integer_* Integer_% string_+ string_- Integer_* Integer_% test1 test2)
;;; (string_+ string_- Integer_* Integer_% string_+ string_- Integer_* Integer_% test1 test2)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ string_- string_* string_% string_+ Integer_- Integer_* Integer_% test2 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- string_* Integer_% Ineger_+ Integer_- string_* string_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ Integer_- string_* Integer_% string_+ Integer_- string_* Integer_% test1 test1)
;;; (string_+ string_- Integer_* Integer_% string_+ string_- Integer_* Integer_% test1 test2)
;;; (string_+ string_- Integer_* Integer_% string_+ string_- Integer_* Integer_% test1 test2)
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
  (map #(if (<= (rand) mutation-rate)
             (vector (rand-nth instructions))
             %) 
          plushy))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.propel-ast/bit-mutation</span>","value":"#'gp.propel-ast/bit-mutation"}
;; <=

;; @@
(bit-mutation (concat plushy-a plushy-b) 0.6)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>string_+</span>","value":"string_+"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>string_includes?</span>","value":"string_includes?"}],"value":"[string_includes?]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>string_absolute</span>","value":"string_absolute"}],"value":"[string_absolute]"},{"type":"html","content":"<span class='clj-symbol'>string_%</span>","value":"string_%"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>boolean_and</span>","value":"boolean_and"}],"value":"[boolean_and]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>in1</span>","value":"in1"}],"value":"[in1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>boolean_and</span>","value":"boolean_and"}],"value":"[boolean_and]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"[false]"},{"type":"html","content":"<span class='clj-symbol'>test1</span>","value":"test1"},{"type":"html","content":"<span class='clj-symbol'>test2</span>","value":"test2"},{"type":"html","content":"<span class='clj-symbol'>Ineger_+</span>","value":"Ineger_+"},{"type":"html","content":"<span class='clj-symbol'>Integer_-</span>","value":"Integer_-"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>integer_=</span>","value":"integer_="}],"value":"[integer_=]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[0]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>boolean_or</span>","value":"boolean_or"}],"value":"[boolean_or]"},{"type":"html","content":"<span class='clj-symbol'>Integer_-</span>","value":"Integer_-"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[0]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[1]"},{"type":"html","content":"<span class='clj-symbol'>test2</span>","value":"test2"},{"type":"html","content":"<span class='clj-symbol'>test1</span>","value":"test1"}],"value":"(string_+ [string_includes?] [string_absolute] string_% [boolean_and] [in1] [boolean_and] [false] test1 test2 Ineger_+ Integer_- [integer_=] [0] [boolean_or] Integer_- [0] [1] test2 test1)"}
;; <=

;; @@

;; @@

;; @@

;; @@
