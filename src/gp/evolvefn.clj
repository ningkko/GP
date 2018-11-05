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


(def function-table (zipmap '(min max + - *)
                            '(2 2 2 2 2)))

;;{* 2, - 2, + 2, max 2, min 2}



(defn random-function 
  []
  (rand-nth (keys function-table)))


(defn random-terminal
  []
  (rand-nth (list 'x (- (rand-int 10) 5))))

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
    (cons (random-function) (repeatedly (get function-table (random-function))
                          #(random-code (dec depth))))))

;;>>(random-code 4)
;;>>(random-code 3)
;;>>(random-code 2)
;;(* (min (* (max -1 0) x) (min (+ x -2) (- 3 -4))) (max (- (+ x x) (* x 4)) 0))
;;(+ (max (* x x) (+ x x)) (* (- x 2) (* 0 x)))
;;0

(defn error 
  [input]
  (let [data-set (eval (list 'fn '[x] input))]
    (Math/sqrt (reduce + (map (fn [[x y]]
                                (Math/abs 
                       			(- (Math/pow (data-set x) 2)
                          		(* y y))))
                   			target-data)))))


;;>>(let [i (random-code 3)] 
;;>>  (println (error i)) "from function set " i)
;;142.64291079475348
;;(* -4 (- -1 (max x x)))



;; the code can be nested vector or just one variable
(defn code-size [code]
  (if (seq? code)
    (count (flatten code));;put thingg all into one seq and return the size
    1));;else size=1




;; mutation and crossover

(defn random-subtree 
  [code]
  
  (if (zero? (rand-int (code-size code)))
    code
    (random-subtree 
      (rand-nth
        (apply concat
               (map #(repeat (code-size %) %)
                    (rest code)))))))


(defn replace-random-subtree
  [code replacement]
  (if (zero? (rand-int (code-size code)))
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



;; use an int depends on the code-size instead of arbitrary 2.

(defn mutate
  [code]
  (replace-random-subtree code 
                          (random-code (int (/ (code-size code) 3)))))



;; crossover subtrees of p1 and p2
(defn crossover
  [parent1 parent2]
  (replace-random-subtree (random-subtree parent1) (random-subtree parent2)))



(defn sort-by-error
  [population]
  (vec (map second
            (sort( fn [[err1 ind1] [err2 ind2]]
                  (< err1 err2))
            (map #(vector (error %) %)
                 population)))))

(defn select
  [population tournament-size]
  
  (nth population
         (apply min (repeatedly tournament-size 
                                #(rand-int (count population))))))



(defn evolve
  [popsize]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(random-code 4)))]
    (let [best (first population)
          best-error (error best)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "     Median error:" (error (nth population 
                                                (int (/ popsize 2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      (if (< best-error 0.1) ;; good enough to count as success
        (println "Success:" best)
        (recur 
          (inc generation)
          (sort-by-error      
            (concat
              (repeatedly (* 1/2 popsize) #(mutate (select population 7)))
              (repeatedly (* 1/4 popsize) #(crossover (select population 7)
                                                     (select population 7)))
              (repeatedly (* 1/4 popsize) #(select population 7)))))))))


(evolve 1000)
