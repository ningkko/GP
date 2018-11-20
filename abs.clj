;; gorilla-repl.fileformat = 1

;; @@
;; revised version of lee's gp.evolvefn
;; has slightly different ways of calculating errors, selection...
;; looks for the right function to get the abs value of an input x
(ns gp.evolvefn)

(def target-data
  (map #(vector % (Math/abs %))
       (range -10 10 0.5)))

;;([-10 10] [-9.5 9.5] [-9.0 9.0] [-8.5 8.5] [-8.0 8.0] 
;;[-7.5 7.5] [-7.0 7.0] [-6.5 6.5] [-6.0 6.0] [-5.5 5.5] 
;;[-5.0 5.0] [-4.5 4.5] [-4.0 4.0] [-3.5 3.5] [-3.0 3.0] 
;;[-2.5 2.5] [-2.0 2.0] [-1.5 1.5] [-1.0 1.0] [-0.5 0.5] 
;;[0.0 0.0] [0.5 0.5] [1.0 1.0] [1.5 1.5] [2.0 2.0] [2.5 2.5] 
;;[3.0 3.0] [3.5 3.5] [4.0 4.0] [4.5 4.5] [5.0 5.0] [5.5 5.5] 
;;[6.0 6.0] [6.5 6.5] [7.0 7.0] [7.5 7.5] [8.0 8.0] [8.5 8.5] 
;;[9.0 9.0] [9.5 9.5])


(def function-table (zipmap '(inc dec min max + - *)
                            '(1 1 2 2 2 2 2)))

;;{inc 1, dec 1, * 2, - 2, + 2, max 2, min 2}



(defn random-function 
  []
  (rand-nth (keys function-table)))


(defn random-terminal
  []
  (rand-nth (list 'x (- (rand-int 20) 10))))

;;>>(random-terminal)
;;>>(random-terminal)
;;>>(random-terminal)
;;-3
;;4
;;x

(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int (count function-table))))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get function-table f)
                          #(random-code (dec depth)))))))


;;>>(random-code 4)
;;>>(random-code 3)
;;>>(random-code 2)
;;(dec (max (dec (max -6 -7)) (dec x)))
;;(inc -7)
;;(dec x)


;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>dec</span>","value":"dec"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(dec x)"}
;; <=

;; @@


;; standard error
(defn error 
  [input]
  (let [data-set (eval (list 'fn '[x] input))]
    (Math/sqrt (reduce + (map (fn [[x y]]
                       			(Math/pow (- (data-set x) 
                                             y) 
                                          2))
                   			target-data)))))


;;(let [i (random-code 3)] 
;;  (println (error i)) "from function set " i)

;;(dec (dec (dec -4)))
;;78.07048097712733




;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.evolvefn/error</span>","value":"#'gp.evolvefn/error"}
;; <=

;; @@
;; the code can be nested vector or just one variable
(defn code-size [code]
  (if (seq? code)
    (count (flatten code));;put things into one seq and return the size
    1));;else size=1

;;(code-size '(inc (x)))
;;2



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.evolvefn/code-size</span>","value":"#'gp.evolvefn/code-size"}
;; <=

;; @@


;; mutation and crossover

(defn random-subtree 
  [code]
  
  (if (zero? (rand-int (code-size code)))
    code
    (random-subtree 
      ( if (odd? (rand-int (code-size code)))
        
        (random-code (rand-int (code-size code)))
        (rand-nth
          (apply conj
                 (map #(repeat (code-size %) %)
                      (rest code))))))))

;;(rand-nth (apply concat (map #(repeat (code-size %) %)
;;                             (rest '(inc (max (+ 1 2) 3))))))

;;(max (+ 1 2) 3)
;;-----------------------------------------------------------------------------------
;;(apply concat (map #(repeat (code-size %) %)
;;                             (rest '(inc (max (+ 1 2) 3)))))
;;((max (+ 1 2) 3) (max (+ 1 2) 3) (max (+ 1 2) 3) (max (+ 1 2) 3) (max (+ 1 2) 3))

;;-----------------------------------------------------------------------------------
;;(map #(repeat (code-size %) %)
;;(rest '(inc (max (+ 1 2) 3))))

;;(((max (+ 1 2) 3) (max (+ 1 2) 3) (max (+ 1 2) 3) (max (+ 1 2) 3) (max (+ 1 2) 3)))
;;map #(repeat (code-size %) %)
;;(rest '(inc (max (+ 1 2) 3)))

;;(conj '(x) '(1) '(random-code 2) '(y))
;;(concat '(x) '(1) '(random-code 2) '(y))

(random-subtree '(inc (max (+ 1 2) 3)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(2)</span>","value":"(2)"}
;; <=

;; @@
(defn replace-random-subtree
  [code replacement]
  (if (zero? (rand-int (code-size code)));; larger the code size, less likely to be replaced
    replacement
  
    (let [position-to-change 
          (rand-nth 
            (apply concat
                   (map #(repeat (code-size %1) %2)
                        (rest code)
                        (iterate inc 1))))]
          (map #(if %1 (replace-random-subtree %2 replacement) %2)
               (for [n (iterate inc 0)] (= n position-to-change))
               code))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.evolvefn/replace-random-subtree</span>","value":"#'gp.evolvefn/replace-random-subtree"}
;; <=

;; @@
;; use an int depends on the code-size instead of arbitrary 2.

(defn mutate
  [code]
  (replace-random-subtree code 
                          (random-code (int (Math/sqrt (code-size code) )))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.evolvefn/mutate</span>","value":"#'gp.evolvefn/mutate"}
;; <=

;; @@

;; crossover subtrees of p1 and p2
(defn crossover
  [parent1 parent2]
  (replace-random-subtree (random-subtree parent1) (random-subtree parent2)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.evolvefn/crossover</span>","value":"#'gp.evolvefn/crossover"}
;; <=

;; @@
(defn sort-by-error
  [population]
  (vec (map second
            (sort( fn [[err1 ind1] [err2 ind2]]
                  (< err1 err2))
            (map #(vector (error %) %)
                 population)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.evolvefn/sort-by-error</span>","value":"#'gp.evolvefn/sort-by-error"}
;; <=

;; @@
(defn select
  [population tournament-size]
  
  (nth population
         (apply min (repeatedly tournament-size 
                                #(rand-int (count population))))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.evolvefn/select</span>","value":"#'gp.evolvefn/select"}
;; <=

;; @@

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}
;; <=

;; @@

(defn evolve
  [popsize]
  (println "Starting evolution...")
  (loop [generation 0 
         population (sort-by-error (repeatedly popsize 
                                               #(random-code 3)))]
    
    (let [best (first population)
          best-error (error best)
          median-error (error (nth population 
                                   (int (/ popsize 2))))]
      
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "Median error:" median-error)
      (println "Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      
      (if (and (zero? best-error)
              (zero? median-error));; (= 0 median-error) never true since 0.0 != 0. 
        ;; output best-error=0, median error=0??
          
        (println "Success:" best)
        (recur 
          (inc generation)
          (sort-by-error      
            (concat
              (repeatedly (* 1/2 popsize) #(mutate (select population 4)))
              (repeatedly (* 1/4 popsize) #(crossover (select population 4)
                                                     (select population 4)))
              (repeatedly (* 1/4 popsize) #(select population 4)))))))))


(evolve 1000)
;; @@
;; ->
;;; Starting evolution...
;;; ======================
;;; Generation: 0
;;; Best error: 13.95752485220786
;;; Best program: (max (min 4 (* x x)) (inc x))
;;; Median error: 65.84071688552609
;;; Average program size: 7.642
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.evolvefn/evolve</span>","value":"#'gp.evolvefn/evolve"}
;; <=

;; @@

;; @@
