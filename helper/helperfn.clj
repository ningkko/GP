;; gorilla-repl.fileformat = 1

;; **
;;; # helper functions
;;; 
;; **

;; @@
(ns helperfn)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(type (Math/sqrt 4.0))
(rand-nth [true false])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}
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

;;remove
(remove pos? [1 -2 2 -1 3 7 0])

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>-2</span>","value":"-2"},{"type":"html","content":"<span class='clj-long'>-1</span>","value":"-1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(-2 -1 0)"}
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
(def instruction ['plus 'minus 'integer_+])
(type 
  (nth 
    (repeatedly ;;repeats functions
      (+ 1 (rand-int (count instruction))) 
      #(rand-nth instruction)) 0))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-class'>clojure.lang.Symbol</span>","value":"clojure.lang.Symbol"}
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
(Math/abs -1)
(Math/PI)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>3.141592653589793</span>","value":"3.141592653589793"}
;; <=

;; @@
(Math/E)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>2.718281828459045</span>","value":"2.718281828459045"}
;; <=

;; @@
(Math/cbrt 3)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>1.4422495703074083</span>","value":"1.4422495703074083"}
;; <=

;; @@
(type 0.1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-class'>java.lang.Double</span>","value":"java.lang.Double"}
;; <=

;; @@
(seq? (range -10.0 11.0))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(quot 1.0 1.1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}
;; <=

;; @@
(zero? 0.0)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(list '(1 2 3))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"(1 2 3)"}],"value":"((1 2 3))"}
;; <=

;; @@
(> Float/MAX_VALUE Integer/MAX_VALUE)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(def cases '((1 2) (0 2) (4 4)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/cases</span>","value":"#'ast/cases"}
;; <=

;; @@
(filter #(not (= '(1 2) %))  cases)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"(0 2)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"(4 4)"}],"value":"((0 2) (4 4))"}
;; <=
