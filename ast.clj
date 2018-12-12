;; gorilla-repl.fileformat = 1

;; @@
(comment
 1. bitmutation keeps doubling plushy-size
  3. lexicase selection cases
  4. lexicase based on category?)

(ns gp.ast
  (:gen-class)
  (:require [gorilla-plot.core :as plot]
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

(def data-addr "src/training_set_metadata.csv")

;;;;;;;;;
;; input and target

(defn read-column 
  [filename column-index]
  (with-open [reader (io/reader filename)]
    (let [data (csv/read-csv reader)]
      (doall
        (map #(nth % column-index) data)))))


(defn read-row 
  [filename row-index]
  (with-open [reader (io/reader filename)]
      (nth (csv/read-csv reader) row-index)))

(defn to-float 
  [input]
  (let [evaluated-input (read-string input)]
    (if (= clojure.lang.Symbol (type evaluated-input))
      (float 0.0)
      (float evaluated-input))))

(defn get-input
  "gets all input from the file"
  [filename]
  (rest (map #(map to-float (drop-last (read-row filename %))) 
             (range (count (read-column filename 0))))))
(defn get-target
  "gets all target from the file"
  [file-name]
  (doall
    (map #(float (read-string %))
         (rest (read-column file-name 11)))))

(defn get-sample-input
  "gets a small sample of input"
  [filename size]
  (rest (map #(map to-float (drop-last (read-row filename %))) 
             (range size))))

(defn get-sample-target
  "gets a small sample of target"
  [file-name size]
  (doall
    (map to-float
         (rest (take (inc size) (read-column file-name 11))))))

(defn write-data
  "writes data into a given directory"
  [out-file data]
  (if (.exists (io/as-file out-file))
     (spit out-file(vec (concat (read-string (slurp out-file)) data)))
    (spit out-file data)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/write-data</span>","value":"#'gp.ast/write-data"}
;; <=

;; @@
(def input (get-sample-input data-addr 100))

(def target (get-sample-target data-addr 100))

(def target-type (apply vector (distinct target)))

;;target-type

;
;Input has 11 columns, with the first column as obk=ject id, and the last column as target.
;So we have 10 actual inputs
;
;;(count (first input))
;;input
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/target-type</span>","value":"#'gp.ast/target-type"}
;; <=

;; @@
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def default-instructions
  (list
	'in1
    'exec_dup
    'exec_if
    'boolean_and
    'boolean_or
    'boolean_not
    'boolean_=
    'close
    true
    false
  ;;============== new functions ================
   
    'pi
    'e
    'float_negative
    'float_positive
    'float_absolute
    'float_sqrt
    'float_cbrt
    'float_+
    'float_-
    'float_*
    'float_%
    'float_=
   	;'random-coefficient
   ))


(def example-push-state
  {:exec '()
   :float '(1.0 2.0 3.0 4.0 5.0 6.0 7.0)
   :input {:in1 4}})


(def opens ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1 
   'exec_if 2})


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/opens</span>","value":"#'gp.ast/opens"}
;; <=

;; @@
;;;;;;;;;
;; Utilities


(def empty-push-state
  {:exec '()
   :float '()
   :boolean '()
   :input {}})


(defn abs
  "Absolute value."
  [x]
  (if (neg? x)
    (- x)
    x))

(defn not-lazy
  "Returns lst if it is not a list, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn lazy-contains? [col key]
  "does a lazy-seq contain something?"
  (some #{key} col))

(defn square 
  "square fucntion"
  [n]
  (* n n))

(defn exp 
  "x^(10^n)"
  [x n]
  (* x (reduce * (repeat n 10))))


(defn push-to-stack
  "Pushes item onto stack in state"
  [state stack item]
  (update state stack conj item))


(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))


(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))))


(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))


(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state and :args is a list of args from the stacks. If there
  aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))


(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/make-push-instruction</span>","value":"#'gp.ast/make-push-instruction"}
;; <=

;; @@


;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in1 (:input state))))


(def random-coefficient (exp (rand) (rand-int 2)))

(defn float_+
  [state]
  (make-push-instruction state +' [:float :float] :float))

(defn float_-
  [state]
  (make-push-instruction state -' [:float :float] :float))

(defn float_*
  [state]
  (make-push-instruction state *' [:float :float] :float))


(defn float_%
  [state]
  (make-push-instruction state
                         (fn [f1 f2]
                           (if (or (nil? f1)) (nil? f2))
                           (float 0.0)
                           (if (zero? f2)
                             f1
                             (quot f1 f2)))
                         [:float :float]
                         :float))

(defn float_=
  [state]
  (make-push-instruction state = [:float :float] :boolean))


(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn exec_if
  [state]
  (make-push-instruction state
                         #(if %1 %3 %2)
                         [:boolean :exec :exec]
                         :exec))


(defn boolean_and
  [state]
  (make-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (make-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (make-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (make-push-instruction state = [:boolean :boolean] :boolean))



(def pi Math/PI)

(def e Math/E)

(defn float_=
  [state]
  (make-push-instruction state = [:float :float] :boolean))



(defn float_absolute
  [state]
  (make-push-instruction state
                         #(float(Math/abs %))
                         [:float]
                         :float))

(defn float_negative
  [state]
  (make-push-instruction state
                         #(neg? %)
                         [:float]
                         :boolean))


(defn float_positive
  [state]
  (make-push-instruction state
                         #(pos? %)
                         [:float]
                         :boolean))

(defn float_sqrt
  [state]
  (make-push-instruction state
                         #(do
                            (float (Math/sqrt (float %))))
                         [:float]
                         :float))

(defn float_cbrt
  [state]
  (make-push-instruction state
                         #(float (Math/cbrt (float %)))
                         [:float]
                         [:float]))



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/float_cbrt</span>","value":"#'gp.ast/float_cbrt"}
;; <=

;; @@

;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        first-raw (first (:exec state))
        first-instruction (if (symbol? first-raw) 
                            (eval first-raw)
                            first-raw)]
    (cond
      (fn? first-instruction) 
      (first-instruction popped-state)
      ;
      (float? first-instruction) 
      (push-to-stack popped-state :float first-instruction)
      ;
      (seq? first-instruction)
      (update popped-state :exec #(concat %2 %1) first-instruction)
      ;
      (or (= first-instruction true) (= first-instruction false))
      (push-to-stack popped-state :boolean first-instruction)
      ;
      :else 
      (throw (Exception. (str "Unrecognized Push instruction in program: " 
                              first-instruction))))))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))

(defn push-from-plushy
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opens
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opens %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close) 
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/push-from-plushy</span>","value":"#'gp.ast/push-from-plushy"}
;; <=

;; @@
;;;;;;;;;
;; GP

(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))


(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))


(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))



(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))


(defn uniform-crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))



(defn multipoint-crossover
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



(defn multipoint-parallel
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


(defn crossover
  [plushy-a plushy-b argmap]
  (case (:crossover argmap)
    :uniform-crossover (uniform-crossover plushy-a plushy-b)
    :multipoint-crossover (multipoint-crossover plushy-a plushy-b)
    :multipoint-parallel (multipoint-parallel plushy-a plushy-b)))

(defn bit-mutation
  "see definition above. Mutation rate [0 1)"
  [plushy mutation-rate instructions]
  (loop [p plushy
         result []]

    (if (empty? p)
      result
      (recur (rest p)
             (conj result 
                     (if (<= (rand) mutation-rate)
                       (rand-nth instructions)
                       (first p)))))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy instructions mutation-rate]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) mutation-rate)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy mutation-rate]
  (remove (fn [x] (< (rand) mutation-rate))
          plushy))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (let[crossed-plushy (crossover 
                                         (:plushy (select-parent pop argmap)) 
                                         (:plushy (select-parent pop argmap))
                                         argmap)]
                      (if (:bit-mutation argmap)
                        (bit-mutation crossed-plushy (:mutation-rate argmap) (:instructions argmap))
                        crossed-plushy))
       
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap)
                                       (:mutation-rate argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap))
                               (:mutation-rate argmap))))})



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/new-individual</span>","value":"#'gp.ast/new-individual"}
;; <=

;; @@
;;------------------------------------------------------------------------------------------
(defn report
  "Reports information each generation."
  [pop generation filename]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
    ;(write-data (vector (:total-error best)) filename)
    (println "Best errors:" (:errors best))
    (println "Best behaviors:" (:behaviors best))
    (println)))

(defn propel-gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions 
           max-initial-plushy-size]
    :as argmap}]
  (println "Starting GP with args:" argmap)
  (loop [generation 0
         population (repeatedly
                     population-size
                     #(hash-map :plushy
                                (make-random-plushy instructions
                                                    max-initial-plushy-size)))
         last-total-error 0];; also check to prevent same total-error all the time
    
    (let [evaluated-pop (sort-by :total-error 
          ;;========================================================================
                                 (map (partial error-function argmap last-total-error)
                                      population))]
      
      ;;===================================================================================
      (report evaluated-pop generation (:out-file argmap))
      (cond
        
        
        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size #(new-individual evaluated-pop argmap))
                     (:total-error (first evaluated-pop)))))))


(defn regression-error-function
  "Finds the behaviors and errors of the individual."
  [argmap last-total-error individual]
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


        errors (map (fn [correct-output output]
                                  (cond
                                   (= output :no-stack-item) (float 2000000)
                                   (not (= nil (some #{:in1} (:plushy individual)))) 
                                    (float 2000000);; check if the answer contains the input
                                   
                                   :else (abs (- correct-output output))))
                                correct-outputs
                                outputs)]
    
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (let [total-err (apply +' errors)]
                          (cond
                            (Double/isNaN total-err) (float 1000000)
                           ; (= (rand-nth errors) (/ total-err (count errors))) (float 2000000)
                            (= last-total-error total-err) (float 3000000)
                            (= (rand-nth outputs) (rand-nth outputs)) (float 2000000)
                            :else total-err)))))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'gp.ast)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :error-function regression-error-function
                                  :max-generations 200
                                  :population-size 200
                                  :max-initial-plushy-size 40
                                  :step-limit 10
                                  :parent-selection :tournament
                                  :tournament-size 8
                                  :mutation-rate 0.06
                                  :crossover :uniform-crossover
                                  :bit-mutation false
                                  :out-file "error/lexicase.csv"}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/-main</span>","value":"#'gp.ast/-main"}
;; <=

;; @@
(-main)
;; @@
;; ->
;;; Starting GP with args: {:max-initial-plushy-size 40, :bit-mutation false, :crossover :uniform-crossover, :mutation-rate 0.06, :instructions (in1 exec_dup exec_if boolean_and boolean_or boolean_not boolean_= close true false pi e float_negative float_positive float_absolute float_sqrt float_cbrt float_+ float_- float_* float_% float_=), :max-generations 200, :parent-selection :tournament, :tournament-size 8, :out-file error/lexicase.csv, :step-limit 10, :error-function #function[gp.ast/regression-error-function], :population-size 200}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_- float_+ float_sqrt e float_+ float_- float_- pi float_positive pi float_positive float_cbrt boolean_not float_% float_positive float_* boolean_and close e false boolean_= false float_+ boolean_and)
;;; Best program: (in1 float_- float_+ float_sqrt e float_+ float_- float_- pi float_positive pi float_positive float_cbrt boolean_not float_% float_positive float_* boolean_and e false boolean_= false float_+ boolean_and)
;;; Best total error: 6723.86360007152
;;; Best errors: (92.0 86.37329995632172 41.77380000054836 89.71869999170303 89.758499994874 65.0 89.81800000369549 41.298600018024445 89.67710000276566 65.0 89.81000000238419 41.459100008010864 39.2525999546051 89.42640000581741 65.0 16.0 66.85310000181198 66.87260000407696 41.81820000708103 93.87870001792908 86.56229996681213 59.323400020599365 87.53560000658035 41.94440000131726 16.0 14.448800027370453 41.86779999732971 89.77949999272823 89.40049999952316 89.71299999952316 41.751300007104874 89.73149999976158 65.0 89.6017000079155 89.43330001831055 41.91499999910593 89.57550001144409 87.86630000174046 87.48240000009537 16.0 89.81239999830723 61.91799999773502 16.0 89.51919999718666 65.0 89.38639998435974 16.0 65.0 65.0 86.72809994220734 89.88889999687672 65.0 41.92809999734163 65.0 93.88380002975464 89.32749998569489 65.0 89.77670000493526 87.3982999920845 51.82889999449253 65.0 41.812800005078316 16.0 41.8585000038147 89.49049997329712 61.782900005578995 51.472100019454956 65.0 89.53949999809265 92.0 65.0 87.55299997329712 51.48079997301102 89.43760001659393 89.6550999879837 89.6884999871254 89.61840000748634 65.0 65.0 51.42720001935959 89.73660001158714 87.76410000026226 93.83329999446869 89.6347000002861 89.94330000132322 89.67620000243187 89.6487999856472 87.67300009727478 65.0 16.0 89.7272999882698 16.0 49.68210005760193 89.65290001034737 89.52709999680519 16.0 16.0 89.81949999928474 51.61050000786781)
;;; Best behaviors: (0.0 1.6267 0.2262 0.2813 0.2415 0.0 0.182 0.7014 0.3229 0.0 0.19 0.5409 2.7474 0.5736 0.0 0.0 0.1469 0.1274 0.1818 1.1213 1.4377 2.6766 0.4644 0.0556 0.0 0.5512 0.1322 0.2205 0.5995 0.287 0.2487 0.2685 0.0 0.3983 0.5667 0.085 0.4245 0.1337 0.5176 0.0 0.1876 0.082 0.0 0.4808 0.0 0.6136 0.0 0.0 0.0 1.2719 0.1111 0.0 0.0719 0.0 1.1162 0.6725 0.0 0.2233 0.6017 0.1711 0.0 0.1872 0.0 0.1415 0.5095 0.2171 0.5279 0.0 0.4605 0.0 0.0 2.447 0.5192 0.5624 0.3449 0.3115 0.3816 0.0 0.0 0.5728 0.2634 0.2359 1.1667 0.3653 0.0567 0.3238 0.3512 2.327 0.0 0.0 0.2727 0.0 2.3179 0.3471 0.4729 0.0 0.0 0.1805 0.3895)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: (e in1 boolean_and float_= boolean_= in1 e float_- false close float_negative exec_if exec_if float_+ float_cbrt float_- false exec_if boolean_or exec_if float_cbrt)
;;; Best program: (e in1 boolean_and float_= boolean_= in1 e float_- false float_negative exec_if (exec_if (float_+ float_cbrt float_- false exec_if (boolean_or exec_if (float_cbrt) ()) ()) ()) ())
;;; Best total error: 6732.686699885875
;;; Best errors: (92.0 86.18190002441406 41.76800000667572 89.69629999995232 89.80660000443459 65.0 89.8648000061512 41.31430000066757 89.69119998812675 65.0 89.84839999675751 41.83050000667572 41.76399999856949 89.54589998722076 65.0 16.0 66.84610000252724 66.89310000091791 41.83900000154972 93.01240003108978 86.86699998378754 61.80310000479221 87.51669999957085 41.943900000303984 16.0 14.485099971294403 41.880300000309944 89.76669999957085 89.40810000896454 89.69470000267029 41.868499994277954 89.67989999055862 65.0 89.61370000243187 89.43199998140335 41.91740000247955 89.57010000944138 87.8669999986887 84.55489993095398 16.0 89.73719999194145 61.917000003159046 16.0 89.6220999956131 65.0 89.37239998579025 16.0 65.0 65.0 86.59689998626709 89.78620000183582 65.0 41.83670000731945 65.0 93.91670000553131 89.31699997186661 65.0 89.44480001926422 87.39480000734329 51.83009999990463 65.0 41.8222000002861 16.0 41.847100004553795 89.66879999637604 61.8585000038147 51.77930000424385 65.0 89.50799998641014 92.0 65.0 89.79809999465942 51.9244000017643 89.48049998283386 89.65049999952316 89.70710000395775 89.55309998989105 65.0 65.0 51.65659999847412 89.71880000829697 87.71180000901222 93.81029999256134 89.6162999868393 89.79880000650883 89.66089999675751 89.65909999608994 89.53470000624657 65.0 16.0 89.70730000734329 16.0 51.88069999963045 89.68259999155998 89.64910000562668 16.0 16.0 89.7977000027895 51.66089999675751)
;;; Best behaviors: (0.0 1.8181 0.232 0.3037 0.1934 0.0 0.1352 0.6857 0.3088 0.0 0.1516 0.1695 0.236 0.4541 0.0 0.0 0.1539 0.1069 0.161 1.9876 1.133 0.1969 0.4833 0.0561 0.0 0.5149 0.1197 0.2333 0.5919 0.3053 0.1315 0.3201 0.0 0.3863 0.568 0.0826 0.4299 0.133 3.4451 0.0 0.2628 0.083 0.0 0.3779 0.0 0.6276 0.0 0.0 0.0 1.4031 0.2138 0.0 0.1633 0.0 1.0833 0.683 0.0 0.5552 0.6052 0.1699 0.0 0.1778 0.0 0.1529 0.3312 0.1415 0.2207 0.0 0.492 0.0 0.0 0.2019 0.0756 0.5195 0.3495 0.2929 0.4469 0.0 0.0 0.3434 0.2812 0.2882 1.1897 0.3837 0.2012 0.3391 0.3409 0.4653 0.0 0.0 0.2927 0.0 0.1193 0.3174 0.3509 0.0 0.0 0.2023 0.3391)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: (float_= float_absolute float_- float_+ float_* in1 in1 float_negative e float_absolute in1 e exec_if pi float_% float_+ float_+ e float_negative in1 float_absolute exec_if boolean_or)
;;; Best program: (float_= float_absolute float_- float_+ float_* in1 in1 float_negative e float_absolute in1 e exec_if (pi float_% float_+ float_+ e float_negative in1 float_absolute exec_if (boolean_or) ()) ())
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: (float_cbrt boolean_or boolean_not in1 in1 false exec_dup false true true float_negative pi float_cbrt float_* e exec_dup float_- in1 exec_if float_-)
;;; Best program: (float_cbrt boolean_or boolean_not in1 in1 false exec_dup (false true true float_negative pi float_cbrt float_* e exec_dup (float_- in1 exec_if (float_-) ())))
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: (e boolean_= boolean_and float_- boolean_= in1 float_= float_% boolean_or false boolean_or exec_if float_negative boolean_= float_negative true e float_+ boolean_or e exec_if float_cbrt float_* float_sqrt boolean_not exec_if float_+)
;;; Best program: (e boolean_= boolean_and float_- boolean_= in1 float_= float_% boolean_or false boolean_or exec_if (float_negative boolean_= float_negative true e float_+ boolean_or e exec_if (float_cbrt float_* float_sqrt boolean_not exec_if (float_+) ()) ()) ())
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute float_- float_negative in1 float_negative pi in1 e exec_if true exec_if float_cbrt float_+ e float_cbrt float_+ boolean_and boolean_or float_absolute)
;;; Best program: (float_absolute float_- float_negative in1 float_negative pi in1 e exec_if (true exec_if (float_cbrt float_+ e float_cbrt float_+ boolean_and boolean_or float_absolute) ()) ())
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_* float_* float_% float_* in1 in1 close float_negative close float_- float_= e float_% float_= float_positive float_* false float_+ float_+ e boolean_or float_*)
;;; Best program: (float_* float_* float_* float_% float_* in1 in1 float_negative float_- float_= e float_% float_= float_positive float_* false float_+ float_+ e boolean_or float_*)
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 7
;;; -------------------------------------------------------
;;; Best plushy: (float_- boolean_or float_- in1 in1 boolean_or float_% close true boolean_or boolean_= float_+ false true float_cbrt in1 exec_dup exec_if boolean_or false)
;;; Best program: (float_- boolean_or float_- in1 in1 boolean_or float_% true boolean_or boolean_= float_+ false true float_cbrt in1 exec_dup (exec_if (boolean_or false) ()))
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; -------------------------------------------------------
;;; Best plushy: (e boolean_= float_= float_+ float_cbrt in1 e float_* boolean_or float_absolute float_- true float_+ in1 exec_dup float_- exec_if)
;;; Best program: (e boolean_= float_= float_+ float_cbrt in1 e float_* boolean_or float_absolute float_- true float_+ in1 exec_dup (float_- exec_if () ()))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 9
;;; -------------------------------------------------------
;;; Best plushy: (pi float_cbrt float_- in1 exec_if float_* false boolean_or float_positive boolean_= float_negative float_negative in1 float_cbrt true float_* float_- float_* exec_dup float_sqrt float_* in1 float_%)
;;; Best program: (pi float_cbrt float_- in1 exec_if (float_* false boolean_or float_positive boolean_= float_negative float_negative in1 float_cbrt true float_* float_- float_* exec_dup (float_sqrt float_* in1 float_%)) ())
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 10
;;; -------------------------------------------------------
;;; Best plushy: (float_= false boolean_= float_+ close float_cbrt in1 close in1 boolean_or float_- float_- float_positive e e e float_+ float_+ true float_+ boolean_not pi false float_+ float_negative)
;;; Best program: (float_= false boolean_= float_+ float_cbrt in1 in1 boolean_or float_- float_- float_positive e e e float_+ float_+ true float_+ boolean_not pi false float_+ float_negative)
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 11
;;; -------------------------------------------------------
;;; Best plushy: (float_positive in1 exec_dup boolean_= float_- in1 e in1 float_- boolean_or float_negative float_- e e float_* boolean_and exec_dup true float_positive float_+ float_+ float_negative exec_dup float_* in1 exec_dup pi float_- boolean_= exec_if pi)
;;; Best program: (float_positive in1 exec_dup (boolean_= float_- in1 e in1 float_- boolean_or float_negative float_- e e float_* boolean_and exec_dup (true float_positive float_+ float_+ float_negative exec_dup (float_* in1 exec_dup (pi float_- boolean_= exec_if (pi) ())))))
;;; Best total error: 6732.686699885875
;;; Best errors: (92.0 86.18190002441406 41.76800000667572 89.69629999995232 89.80660000443459 65.0 89.8648000061512 41.31430000066757 89.69119998812675 65.0 89.84839999675751 41.83050000667572 41.76399999856949 89.54589998722076 65.0 16.0 66.84610000252724 66.89310000091791 41.83900000154972 93.01240003108978 86.86699998378754 61.80310000479221 87.51669999957085 41.943900000303984 16.0 14.485099971294403 41.880300000309944 89.76669999957085 89.40810000896454 89.69470000267029 41.868499994277954 89.67989999055862 65.0 89.61370000243187 89.43199998140335 41.91740000247955 89.57010000944138 87.8669999986887 84.55489993095398 16.0 89.73719999194145 61.917000003159046 16.0 89.6220999956131 65.0 89.37239998579025 16.0 65.0 65.0 86.59689998626709 89.78620000183582 65.0 41.83670000731945 65.0 93.91670000553131 89.31699997186661 65.0 89.44480001926422 87.39480000734329 51.83009999990463 65.0 41.8222000002861 16.0 41.847100004553795 89.66879999637604 61.8585000038147 51.77930000424385 65.0 89.50799998641014 92.0 65.0 89.79809999465942 51.9244000017643 89.48049998283386 89.65049999952316 89.70710000395775 89.55309998989105 65.0 65.0 51.65659999847412 89.71880000829697 87.71180000901222 93.81029999256134 89.6162999868393 89.79880000650883 89.66089999675751 89.65909999608994 89.53470000624657 65.0 16.0 89.70730000734329 16.0 51.88069999963045 89.68259999155998 89.64910000562668 16.0 16.0 89.7977000027895 51.66089999675751)
;;; Best behaviors: (0.0 1.8181 0.232 0.3037 0.1934 0.0 0.1352 0.6857 0.3088 0.0 0.1516 0.1695 0.236 0.4541 0.0 0.0 0.1539 0.1069 0.161 1.9876 1.133 0.1969 0.4833 0.0561 0.0 0.5149 0.1197 0.2333 0.5919 0.3053 0.1315 0.3201 0.0 0.3863 0.568 0.0826 0.4299 0.133 3.4451 0.0 0.2628 0.083 0.0 0.3779 0.0 0.6276 0.0 0.0 0.0 1.4031 0.2138 0.0 0.1633 0.0 1.0833 0.683 0.0 0.5552 0.6052 0.1699 0.0 0.1778 0.0 0.1529 0.3312 0.1415 0.2207 0.0 0.492 0.0 0.0 0.2019 0.0756 0.5195 0.3495 0.2929 0.4469 0.0 0.0 0.3434 0.2812 0.2882 1.1897 0.3837 0.2012 0.3391 0.3409 0.4653 0.0 0.0 0.2927 0.0 0.1193 0.3174 0.3509 0.0 0.0 0.2023 0.3391)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 12
;;; -------------------------------------------------------
;;; Best plushy: (float_+ boolean_= float_- float_+ close e in1 close float_* float_sqrt false float_- float_positive float_* boolean_and float_= float_negative float_positive float_+ float_= e boolean_or float_* exec_dup pi)
;;; Best program: (float_+ boolean_= float_- float_+ e in1 float_* float_sqrt false float_- float_positive float_* boolean_and float_= float_negative float_positive float_+ float_= e boolean_or float_* exec_dup (pi))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 13
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_* float_* in1 in1 exec_dup close in1 float_% float_+ float_cbrt in1 float_* float_+ float_* float_+ pi float_+ float_- boolean_or in1 boolean_and float_negative float_- float_-)
;;; Best program: (float_* float_* float_* in1 in1 exec_dup () in1 float_% float_+ float_cbrt in1 float_* float_+ float_* float_+ pi float_+ float_- boolean_or in1 boolean_and float_negative float_- float_-)
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 14
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_absolute boolean_= boolean_and boolean_= in1 in1 boolean_and boolean_and float_negative boolean_or float_negative boolean_or close float_+ float_+ e boolean_or float_- float_+ pi)
;;; Best program: (float_* float_absolute boolean_= boolean_and boolean_= in1 in1 boolean_and boolean_and float_negative boolean_or float_negative boolean_or float_+ float_+ e boolean_or float_- float_+ pi)
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 15
;;; -------------------------------------------------------
;;; Best plushy: (float_+ boolean_= float_* close in1 close boolean_and boolean_and float_negative in1 e close float_+ float_* e true float_+ boolean_or pi float_positive)
;;; Best program: (float_+ boolean_= float_* in1 boolean_and boolean_and float_negative in1 e float_+ float_* e true float_+ boolean_or pi float_positive)
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 16
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= float_* float_absolute boolean_= float_- in1 float_sqrt in1 float_cbrt true true float_cbrt float_negative float_absolute float_% float_+ float_* float_= exec_dup float_- boolean_or float_+ boolean_or pi)
;;; Best program: (boolean_= float_* float_absolute boolean_= float_- in1 float_sqrt in1 float_cbrt true true float_cbrt float_negative float_absolute float_% float_+ float_* float_= exec_dup (float_- boolean_or float_+ boolean_or pi))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 17
;;; -------------------------------------------------------
;;; Best plushy: (pi float_absolute float_+ float_cbrt close e false in1 in1 exec_dup true float_- e e boolean_or float_= float_+ boolean_and float_+ boolean_= boolean_or)
;;; Best program: (pi float_absolute float_+ float_cbrt e false in1 in1 exec_dup (true float_- e e boolean_or float_= float_+ boolean_and float_+ boolean_= boolean_or))
;;; Best total error: 14295.820528313518
;;; Best errors: (257.0460510253906 34.9140625 8.42578125 89.81012700498104 262.7112731933594 29.31640625 257.8467102050781 306.59588623046875 59.4140625 84.4140625 89.03433501720428 304.5 317.44671630859375 269.8052062988281 281.6551818847656 37.96484375 285.3986511230469 279.130126953125 108.8203125 254.61538696289062 62.1171875 61.41048002243042 87.03433501720428 317.81170654296875 13.902542114257812 137.05078125 316.6480712890625 61.69921875 259.6153869628906 54.66796875 305.8618469238281 56.77734375 64.07024800777435 258.59588623046875 55.72265625 10.20703125 257.8618469238281 85.92886996269226 264.3986511230469 343.44671630859375 257.013427734375 8.9140625 133.4140625 88.30543899536133 32.3046875 259.9662170410156 18.1015625 30.72265625 281.6551818847656 87.05063301324844 38.14453125 287.13287353515625 9.12890625 85.8203125 263.3125 35.33203125 30.01953125 37.08984375 270.6363525390625 0.91015625 12.08984375 309.29998779296875 134.99609375 307.9662170410156 36.9140625 284.130126953125 98.64453125 84.58984375 268.6480712890625 40.14453125 282.8618469238281 61.69921875 294.6551818847656 36.2109375 60.1171875 56.25 88.6363639831543 85.99609375 11.9140625 2.66796875 87.92886996269226 62.64453125 264.8052062988281 57.3046875 56.25 259.6153869628906 259.0460510253906 57.12890625 286.3829650878906 330.130126953125 89.03433501720428 343.41558837890625 0.14453125 36.2109375 59.4140625 35.328125 134.8203125 61.69921875 307.41558837890625)
;;; Best behaviors: (349.04605 53.085938 33.57422 0.189873 352.71127 35.683594 347.8467 348.5959 149.41406 149.41406 0.965665 346.5 359.44672 359.8052 346.65518 53.964844 352.39865 346.13013 150.82031 349.6154 150.11719 0.58952 0.965665 359.8117 2.097458 152.05078 358.64807 151.69922 349.6154 35.33203 347.86185 33.222656 0.929752 348.5959 34.277344 52.20703 347.86185 2.07113 352.39865 359.44672 347.01343 53.085938 149.41406 1.694561 32.695312 349.96622 34.101562 34.277344 346.65518 0.949367 51.85547 352.13287 32.871094 150.82031 358.3125 54.66797 34.98047 52.910156 358.63635 52.910156 52.910156 351.3 150.9961 349.96622 53.085938 346.13013 150.64453 149.58984 358.64807 51.85547 347.86185 151.69922 346.65518 53.789062 150.11719 33.75 1.363636 150.9961 53.085938 54.66797 2.07113 150.64453 359.8052 32.695312 33.75 349.6154 349.04605 32.871094 351.38297 346.13013 0.965665 359.4156 51.85547 53.789062 149.41406 51.328125 150.82031 151.69922 359.4156)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 18
;;; -------------------------------------------------------
;;; Best plushy: (boolean_and exec_dup float_- float_+ in1 in1 close float_= float_- float_cbrt close false boolean_or exec_dup in1 float_+ boolean_or)
;;; Best program: (boolean_and exec_dup (float_- float_+ in1 in1) float_= float_- float_cbrt false boolean_or exec_dup (in1 float_+ boolean_or))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 19
;;; -------------------------------------------------------
;;; Best plushy: (float_positive float_absolute float_* in1 boolean_or in1 float_- float_sqrt boolean_and close boolean_and boolean_or float_+ float_- exec_dup exec_dup e float_+ boolean_or float_+)
;;; Best program: (float_positive float_absolute float_* in1 boolean_or in1 float_- float_sqrt boolean_and boolean_and boolean_or float_+ float_- exec_dup (exec_dup (e float_+ boolean_or float_+)))
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 20
;;; -------------------------------------------------------
;;; Best plushy: (boolean_and exec_dup float_- float_+ in1 float_negative in1 close float_= float_- float_cbrt float_= false float_- exec_dup in1 e boolean_or float_+ exec_if)
;;; Best program: (boolean_and exec_dup (float_- float_+ in1 float_negative in1) float_= float_- float_cbrt float_= false float_- exec_dup (in1 e boolean_or float_+ exec_if () ()))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 21
;;; -------------------------------------------------------
;;; Best plushy: (float_= float_absolute float_* in1 boolean_or float_sqrt boolean_and false float_- boolean_or close float_- exec_dup float_* float_* float_+ boolean_or float_+ float_=)
;;; Best program: (float_= float_absolute float_* in1 boolean_or float_sqrt boolean_and false float_- boolean_or float_- exec_dup (float_* float_* float_+ boolean_or float_+ float_=))
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 22
;;; -------------------------------------------------------
;;; Best plushy: (float_= exec_dup float_cbrt float_* in1 float_- float_positive float_cbrt float_- float_sqrt float_- exec_dup float_* boolean_or in1 float_cbrt float_+ e pi)
;;; Best program: (float_= exec_dup (float_cbrt float_* in1 float_- float_positive float_cbrt float_- float_sqrt float_- exec_dup (float_* boolean_or in1 float_cbrt float_+ e pi)))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 23
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_= boolean_and float_* float_- float_+ in1 float_negative in1 boolean_and float_= float_- float_absolute float_= float_- float_- exec_dup in1 float_+ false boolean_= float_+ float_=)
;;; Best program: (in1 float_= boolean_and float_* float_- float_+ in1 float_negative in1 boolean_and float_= float_- float_absolute float_= float_- float_- exec_dup (in1 float_+ false boolean_= float_+ float_=))
;;; Best total error: 6723.86360007152
;;; Best errors: (92.0 86.37329995632172 41.77380000054836 89.71869999170303 89.758499994874 65.0 89.81800000369549 41.298600018024445 89.67710000276566 65.0 89.81000000238419 41.459100008010864 39.2525999546051 89.42640000581741 65.0 16.0 66.85310000181198 66.87260000407696 41.81820000708103 93.87870001792908 86.56229996681213 59.323400020599365 87.53560000658035 41.94440000131726 16.0 14.448800027370453 41.86779999732971 89.77949999272823 89.40049999952316 89.71299999952316 41.751300007104874 89.73149999976158 65.0 89.6017000079155 89.43330001831055 41.91499999910593 89.57550001144409 87.86630000174046 87.48240000009537 16.0 89.81239999830723 61.91799999773502 16.0 89.51919999718666 65.0 89.38639998435974 16.0 65.0 65.0 86.72809994220734 89.88889999687672 65.0 41.92809999734163 65.0 93.88380002975464 89.32749998569489 65.0 89.77670000493526 87.3982999920845 51.82889999449253 65.0 41.812800005078316 16.0 41.8585000038147 89.49049997329712 61.782900005578995 51.472100019454956 65.0 89.53949999809265 92.0 65.0 87.55299997329712 51.48079997301102 89.43760001659393 89.6550999879837 89.6884999871254 89.61840000748634 65.0 65.0 51.42720001935959 89.73660001158714 87.76410000026226 93.83329999446869 89.6347000002861 89.94330000132322 89.67620000243187 89.6487999856472 87.67300009727478 65.0 16.0 89.7272999882698 16.0 49.68210005760193 89.65290001034737 89.52709999680519 16.0 16.0 89.81949999928474 51.61050000786781)
;;; Best behaviors: (0.0 1.6267 0.2262 0.2813 0.2415 0.0 0.182 0.7014 0.3229 0.0 0.19 0.5409 2.7474 0.5736 0.0 0.0 0.1469 0.1274 0.1818 1.1213 1.4377 2.6766 0.4644 0.0556 0.0 0.5512 0.1322 0.2205 0.5995 0.287 0.2487 0.2685 0.0 0.3983 0.5667 0.085 0.4245 0.1337 0.5176 0.0 0.1876 0.082 0.0 0.4808 0.0 0.6136 0.0 0.0 0.0 1.2719 0.1111 0.0 0.0719 0.0 1.1162 0.6725 0.0 0.2233 0.6017 0.1711 0.0 0.1872 0.0 0.1415 0.5095 0.2171 0.5279 0.0 0.4605 0.0 0.0 2.447 0.5192 0.5624 0.3449 0.3115 0.3816 0.0 0.0 0.5728 0.2634 0.2359 1.1667 0.3653 0.0567 0.3238 0.3512 2.327 0.0 0.0 0.2727 0.0 2.3179 0.3471 0.4729 0.0 0.0 0.1805 0.3895)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 24
;;; -------------------------------------------------------
;;; Best plushy: (exec_dup float_- float_% float_+ in1 boolean_or in1 close e boolean_not float_= boolean_or boolean_not boolean_and in1 float_positive boolean_or exec_if pi)
;;; Best program: (exec_dup (float_- float_% float_+ in1 boolean_or in1) e boolean_not float_= boolean_or boolean_not boolean_and in1 float_positive boolean_or exec_if (pi) ())
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 25
;;; -------------------------------------------------------
;;; Best plushy: (float_cbrt in1 float_* float_* float_cbrt boolean_or in1 float_negative float_negative false float_= float_- float_= false exec_dup float_* float_+ boolean_= float_+ false boolean_= true float_cbrt)
;;; Best program: (float_cbrt in1 float_* float_* float_cbrt boolean_or in1 float_negative float_negative false float_= float_- float_= false exec_dup (float_* float_+ boolean_= float_+ false boolean_= true float_cbrt))
;;; Best total error: 6732.686699885875
;;; Best errors: (92.0 86.18190002441406 41.76800000667572 89.69629999995232 89.80660000443459 65.0 89.8648000061512 41.31430000066757 89.69119998812675 65.0 89.84839999675751 41.83050000667572 41.76399999856949 89.54589998722076 65.0 16.0 66.84610000252724 66.89310000091791 41.83900000154972 93.01240003108978 86.86699998378754 61.80310000479221 87.51669999957085 41.943900000303984 16.0 14.485099971294403 41.880300000309944 89.76669999957085 89.40810000896454 89.69470000267029 41.868499994277954 89.67989999055862 65.0 89.61370000243187 89.43199998140335 41.91740000247955 89.57010000944138 87.8669999986887 84.55489993095398 16.0 89.73719999194145 61.917000003159046 16.0 89.6220999956131 65.0 89.37239998579025 16.0 65.0 65.0 86.59689998626709 89.78620000183582 65.0 41.83670000731945 65.0 93.91670000553131 89.31699997186661 65.0 89.44480001926422 87.39480000734329 51.83009999990463 65.0 41.8222000002861 16.0 41.847100004553795 89.66879999637604 61.8585000038147 51.77930000424385 65.0 89.50799998641014 92.0 65.0 89.79809999465942 51.9244000017643 89.48049998283386 89.65049999952316 89.70710000395775 89.55309998989105 65.0 65.0 51.65659999847412 89.71880000829697 87.71180000901222 93.81029999256134 89.6162999868393 89.79880000650883 89.66089999675751 89.65909999608994 89.53470000624657 65.0 16.0 89.70730000734329 16.0 51.88069999963045 89.68259999155998 89.64910000562668 16.0 16.0 89.7977000027895 51.66089999675751)
;;; Best behaviors: (0.0 1.8181 0.232 0.3037 0.1934 0.0 0.1352 0.6857 0.3088 0.0 0.1516 0.1695 0.236 0.4541 0.0 0.0 0.1539 0.1069 0.161 1.9876 1.133 0.1969 0.4833 0.0561 0.0 0.5149 0.1197 0.2333 0.5919 0.3053 0.1315 0.3201 0.0 0.3863 0.568 0.0826 0.4299 0.133 3.4451 0.0 0.2628 0.083 0.0 0.3779 0.0 0.6276 0.0 0.0 0.0 1.4031 0.2138 0.0 0.1633 0.0 1.0833 0.683 0.0 0.5552 0.6052 0.1699 0.0 0.1778 0.0 0.1529 0.3312 0.1415 0.2207 0.0 0.492 0.0 0.0 0.2019 0.0756 0.5195 0.3495 0.2929 0.4469 0.0 0.0 0.3434 0.2812 0.2882 1.1897 0.3837 0.2012 0.3391 0.3409 0.4653 0.0 0.0 0.2927 0.0 0.1193 0.3174 0.3509 0.0 0.0 0.2023 0.3391)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 26
;;; -------------------------------------------------------
;;; Best plushy: (in1 boolean_and float_* float_* in1 boolean_or float_negative in1 false float_- e float_= float_absolute false float_- exec_dup float_+ float_* boolean_not in1 float_+ true float_cbrt)
;;; Best program: (in1 boolean_and float_* float_* in1 boolean_or float_negative in1 false float_- e float_= float_absolute false float_- exec_dup (float_+ float_* boolean_not in1 float_+ true float_cbrt))
;;; Best total error: 6723.86360007152
;;; Best errors: (92.0 86.37329995632172 41.77380000054836 89.71869999170303 89.758499994874 65.0 89.81800000369549 41.298600018024445 89.67710000276566 65.0 89.81000000238419 41.459100008010864 39.2525999546051 89.42640000581741 65.0 16.0 66.85310000181198 66.87260000407696 41.81820000708103 93.87870001792908 86.56229996681213 59.323400020599365 87.53560000658035 41.94440000131726 16.0 14.448800027370453 41.86779999732971 89.77949999272823 89.40049999952316 89.71299999952316 41.751300007104874 89.73149999976158 65.0 89.6017000079155 89.43330001831055 41.91499999910593 89.57550001144409 87.86630000174046 87.48240000009537 16.0 89.81239999830723 61.91799999773502 16.0 89.51919999718666 65.0 89.38639998435974 16.0 65.0 65.0 86.72809994220734 89.88889999687672 65.0 41.92809999734163 65.0 93.88380002975464 89.32749998569489 65.0 89.77670000493526 87.3982999920845 51.82889999449253 65.0 41.812800005078316 16.0 41.8585000038147 89.49049997329712 61.782900005578995 51.472100019454956 65.0 89.53949999809265 92.0 65.0 87.55299997329712 51.48079997301102 89.43760001659393 89.6550999879837 89.6884999871254 89.61840000748634 65.0 65.0 51.42720001935959 89.73660001158714 87.76410000026226 93.83329999446869 89.6347000002861 89.94330000132322 89.67620000243187 89.6487999856472 87.67300009727478 65.0 16.0 89.7272999882698 16.0 49.68210005760193 89.65290001034737 89.52709999680519 16.0 16.0 89.81949999928474 51.61050000786781)
;;; Best behaviors: (0.0 1.6267 0.2262 0.2813 0.2415 0.0 0.182 0.7014 0.3229 0.0 0.19 0.5409 2.7474 0.5736 0.0 0.0 0.1469 0.1274 0.1818 1.1213 1.4377 2.6766 0.4644 0.0556 0.0 0.5512 0.1322 0.2205 0.5995 0.287 0.2487 0.2685 0.0 0.3983 0.5667 0.085 0.4245 0.1337 0.5176 0.0 0.1876 0.082 0.0 0.4808 0.0 0.6136 0.0 0.0 0.0 1.2719 0.1111 0.0 0.0719 0.0 1.1162 0.6725 0.0 0.2233 0.6017 0.1711 0.0 0.1872 0.0 0.1415 0.5095 0.2171 0.5279 0.0 0.4605 0.0 0.0 2.447 0.5192 0.5624 0.3449 0.3115 0.3816 0.0 0.0 0.5728 0.2634 0.2359 1.1667 0.3653 0.0567 0.3238 0.3512 2.327 0.0 0.0 0.2727 0.0 2.3179 0.3471 0.4729 0.0 0.0 0.1805 0.3895)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 27
;;; -------------------------------------------------------
;;; Best plushy: (float_* in1 float_* boolean_or float_* in1 in1 exec_if in1 close float_sqrt false float_positive float_negative e float_- boolean_or close float_* float_* float_* float_* float_+)
;;; Best program: (float_* in1 float_* boolean_or float_* in1 in1 exec_if (in1) (float_sqrt false float_positive float_negative e float_- boolean_or) float_* float_* float_* float_* float_+)
;;; Best total error: 6732.686699885875
;;; Best errors: (92.0 86.18190002441406 41.76800000667572 89.69629999995232 89.80660000443459 65.0 89.8648000061512 41.31430000066757 89.69119998812675 65.0 89.84839999675751 41.83050000667572 41.76399999856949 89.54589998722076 65.0 16.0 66.84610000252724 66.89310000091791 41.83900000154972 93.01240003108978 86.86699998378754 61.80310000479221 87.51669999957085 41.943900000303984 16.0 14.485099971294403 41.880300000309944 89.76669999957085 89.40810000896454 89.69470000267029 41.868499994277954 89.67989999055862 65.0 89.61370000243187 89.43199998140335 41.91740000247955 89.57010000944138 87.8669999986887 84.55489993095398 16.0 89.73719999194145 61.917000003159046 16.0 89.6220999956131 65.0 89.37239998579025 16.0 65.0 65.0 86.59689998626709 89.78620000183582 65.0 41.83670000731945 65.0 93.91670000553131 89.31699997186661 65.0 89.44480001926422 87.39480000734329 51.83009999990463 65.0 41.8222000002861 16.0 41.847100004553795 89.66879999637604 61.8585000038147 51.77930000424385 65.0 89.50799998641014 92.0 65.0 89.79809999465942 51.9244000017643 89.48049998283386 89.65049999952316 89.70710000395775 89.55309998989105 65.0 65.0 51.65659999847412 89.71880000829697 87.71180000901222 93.81029999256134 89.6162999868393 89.79880000650883 89.66089999675751 89.65909999608994 89.53470000624657 65.0 16.0 89.70730000734329 16.0 51.88069999963045 89.68259999155998 89.64910000562668 16.0 16.0 89.7977000027895 51.66089999675751)
;;; Best behaviors: (0.0 1.8181 0.232 0.3037 0.1934 0.0 0.1352 0.6857 0.3088 0.0 0.1516 0.1695 0.236 0.4541 0.0 0.0 0.1539 0.1069 0.161 1.9876 1.133 0.1969 0.4833 0.0561 0.0 0.5149 0.1197 0.2333 0.5919 0.3053 0.1315 0.3201 0.0 0.3863 0.568 0.0826 0.4299 0.133 3.4451 0.0 0.2628 0.083 0.0 0.3779 0.0 0.6276 0.0 0.0 0.0 1.4031 0.2138 0.0 0.1633 0.0 1.0833 0.683 0.0 0.5552 0.6052 0.1699 0.0 0.1778 0.0 0.1529 0.3312 0.1415 0.2207 0.0 0.492 0.0 0.0 0.2019 0.0756 0.5195 0.3495 0.2929 0.4469 0.0 0.0 0.3434 0.2812 0.2882 1.1897 0.3837 0.2012 0.3391 0.3409 0.4653 0.0 0.0 0.2927 0.0 0.1193 0.3174 0.3509 0.0 0.0 0.2023 0.3391)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 28
;;; -------------------------------------------------------
;;; Best plushy: (float_positive float_* true float_negative boolean_and in1 float_- in1 float_cbrt float_- float_negative boolean_and in1 float_* float_* exec_dup float_+ e boolean_= close)
;;; Best program: (float_positive float_* true float_negative boolean_and in1 float_- in1 float_cbrt float_- float_negative boolean_and in1 float_* float_* exec_dup (float_+ e boolean_=))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 29
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_* float_cbrt in1 in1 float_- float_= float_cbrt float_- float_negative in1 float_* float_* float_cbrt float_- float_* boolean_= float_cbrt)
;;; Best program: (float_* float_* float_cbrt in1 in1 float_- float_= float_cbrt float_- float_negative in1 float_* float_* float_cbrt float_- float_* boolean_= float_cbrt)
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 30
;;; -------------------------------------------------------
;;; Best plushy: (float_= float_* exec_dup float_% in1 in1 exec_if boolean_and close false exec_dup boolean_or in1 float_+ float_cbrt float_cbrt float_* float_=)
;;; Best program: (float_= float_* exec_dup (float_% in1 in1 exec_if (boolean_and) (false exec_dup (boolean_or in1 float_+ float_cbrt float_cbrt float_* float_=))))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 31
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_negative float_positive in1 float_- exec_if float_- boolean_and close float_sqrt float_- in1 float_- float_* float_+ float_* float_= float_*)
;;; Best program: (float_* float_negative float_positive in1 float_- exec_if (float_- boolean_and) (float_sqrt float_- in1 float_- float_* float_+ float_* float_= float_*))
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 32
;;; -------------------------------------------------------
;;; Best plushy: (float_* in1 in1 float_negative float_sqrt boolean_and false exec_dup float_positive float_+ float_+ float_= float_* float_+ float_* float_+)
;;; Best program: (float_* in1 in1 float_negative float_sqrt boolean_and false exec_dup (float_positive float_+ float_+ float_= float_* float_+ float_* float_+))
;;; Best total error: 6732.686699885875
;;; Best errors: (92.0 86.18190002441406 41.76800000667572 89.69629999995232 89.80660000443459 65.0 89.8648000061512 41.31430000066757 89.69119998812675 65.0 89.84839999675751 41.83050000667572 41.76399999856949 89.54589998722076 65.0 16.0 66.84610000252724 66.89310000091791 41.83900000154972 93.01240003108978 86.86699998378754 61.80310000479221 87.51669999957085 41.943900000303984 16.0 14.485099971294403 41.880300000309944 89.76669999957085 89.40810000896454 89.69470000267029 41.868499994277954 89.67989999055862 65.0 89.61370000243187 89.43199998140335 41.91740000247955 89.57010000944138 87.8669999986887 84.55489993095398 16.0 89.73719999194145 61.917000003159046 16.0 89.6220999956131 65.0 89.37239998579025 16.0 65.0 65.0 86.59689998626709 89.78620000183582 65.0 41.83670000731945 65.0 93.91670000553131 89.31699997186661 65.0 89.44480001926422 87.39480000734329 51.83009999990463 65.0 41.8222000002861 16.0 41.847100004553795 89.66879999637604 61.8585000038147 51.77930000424385 65.0 89.50799998641014 92.0 65.0 89.79809999465942 51.9244000017643 89.48049998283386 89.65049999952316 89.70710000395775 89.55309998989105 65.0 65.0 51.65659999847412 89.71880000829697 87.71180000901222 93.81029999256134 89.6162999868393 89.79880000650883 89.66089999675751 89.65909999608994 89.53470000624657 65.0 16.0 89.70730000734329 16.0 51.88069999963045 89.68259999155998 89.64910000562668 16.0 16.0 89.7977000027895 51.66089999675751)
;;; Best behaviors: (0.0 1.8181 0.232 0.3037 0.1934 0.0 0.1352 0.6857 0.3088 0.0 0.1516 0.1695 0.236 0.4541 0.0 0.0 0.1539 0.1069 0.161 1.9876 1.133 0.1969 0.4833 0.0561 0.0 0.5149 0.1197 0.2333 0.5919 0.3053 0.1315 0.3201 0.0 0.3863 0.568 0.0826 0.4299 0.133 3.4451 0.0 0.2628 0.083 0.0 0.3779 0.0 0.6276 0.0 0.0 0.0 1.4031 0.2138 0.0 0.1633 0.0 1.0833 0.683 0.0 0.5552 0.6052 0.1699 0.0 0.1778 0.0 0.1529 0.3312 0.1415 0.2207 0.0 0.492 0.0 0.0 0.2019 0.0756 0.5195 0.3495 0.2929 0.4469 0.0 0.0 0.3434 0.2812 0.2882 1.1897 0.3837 0.2012 0.3391 0.3409 0.4653 0.0 0.0 0.2927 0.0 0.1193 0.3174 0.3509 0.0 0.0 0.2023 0.3391)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 33
;;; -------------------------------------------------------
;;; Best plushy: (boolean_and float_+ float_* float_% float_sqrt in1 in1 boolean_and boolean_and false float_- float_- float_- in1 false float_+ exec_dup float_+)
;;; Best program: (boolean_and float_+ float_* float_% float_sqrt in1 in1 boolean_and boolean_and false float_- float_- float_- in1 false float_+ exec_dup (float_+))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 34
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_positive float_cbrt in1 in1 float_sqrt float_- float_absolute boolean_or float_* float_% float_* false in1 exec_if float_+ float_cbrt in1 float_cbrt boolean_or float_+ exec_dup float_+ float_positive float_cbrt float_*)
;;; Best program: (float_* float_positive float_cbrt in1 in1 float_sqrt float_- float_absolute boolean_or float_* float_% float_* false in1 exec_if (float_+ float_cbrt in1 float_cbrt boolean_or float_+ exec_dup (float_+ float_positive float_cbrt float_*)) ())
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 35
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_positive float_cbrt in1 in1 float_sqrt float_- float_absolute boolean_or float_* float_% float_* false in1 exec_if float_+ float_cbrt in1 float_cbrt boolean_or float_+ exec_dup float_+ float_positive float_cbrt float_*)
;;; Best program: (in1 float_* float_positive float_cbrt in1 in1 float_sqrt float_- float_absolute boolean_or float_* float_% float_* false in1 exec_if (float_+ float_cbrt in1 float_cbrt boolean_or float_+ exec_dup (float_+ float_positive float_cbrt float_*)) ())
;;; Best total error: 6723.86360007152
;;; Best errors: (92.0 86.37329995632172 41.77380000054836 89.71869999170303 89.758499994874 65.0 89.81800000369549 41.298600018024445 89.67710000276566 65.0 89.81000000238419 41.459100008010864 39.2525999546051 89.42640000581741 65.0 16.0 66.85310000181198 66.87260000407696 41.81820000708103 93.87870001792908 86.56229996681213 59.323400020599365 87.53560000658035 41.94440000131726 16.0 14.448800027370453 41.86779999732971 89.77949999272823 89.40049999952316 89.71299999952316 41.751300007104874 89.73149999976158 65.0 89.6017000079155 89.43330001831055 41.91499999910593 89.57550001144409 87.86630000174046 87.48240000009537 16.0 89.81239999830723 61.91799999773502 16.0 89.51919999718666 65.0 89.38639998435974 16.0 65.0 65.0 86.72809994220734 89.88889999687672 65.0 41.92809999734163 65.0 93.88380002975464 89.32749998569489 65.0 89.77670000493526 87.3982999920845 51.82889999449253 65.0 41.812800005078316 16.0 41.8585000038147 89.49049997329712 61.782900005578995 51.472100019454956 65.0 89.53949999809265 92.0 65.0 87.55299997329712 51.48079997301102 89.43760001659393 89.6550999879837 89.6884999871254 89.61840000748634 65.0 65.0 51.42720001935959 89.73660001158714 87.76410000026226 93.83329999446869 89.6347000002861 89.94330000132322 89.67620000243187 89.6487999856472 87.67300009727478 65.0 16.0 89.7272999882698 16.0 49.68210005760193 89.65290001034737 89.52709999680519 16.0 16.0 89.81949999928474 51.61050000786781)
;;; Best behaviors: (0.0 1.6267 0.2262 0.2813 0.2415 0.0 0.182 0.7014 0.3229 0.0 0.19 0.5409 2.7474 0.5736 0.0 0.0 0.1469 0.1274 0.1818 1.1213 1.4377 2.6766 0.4644 0.0556 0.0 0.5512 0.1322 0.2205 0.5995 0.287 0.2487 0.2685 0.0 0.3983 0.5667 0.085 0.4245 0.1337 0.5176 0.0 0.1876 0.082 0.0 0.4808 0.0 0.6136 0.0 0.0 0.0 1.2719 0.1111 0.0 0.0719 0.0 1.1162 0.6725 0.0 0.2233 0.6017 0.1711 0.0 0.1872 0.0 0.1415 0.5095 0.2171 0.5279 0.0 0.4605 0.0 0.0 2.447 0.5192 0.5624 0.3449 0.3115 0.3816 0.0 0.0 0.5728 0.2634 0.2359 1.1667 0.3653 0.0567 0.3238 0.3512 2.327 0.0 0.0 0.2727 0.0 2.3179 0.3471 0.4729 0.0 0.0 0.1805 0.3895)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 36
;;; -------------------------------------------------------
;;; Best plushy: (exec_dup boolean_not float_cbrt float_cbrt in1 in1 in1 boolean_and float_positive close float_- float_- in1 false float_* exec_dup float_- exec_if)
;;; Best program: (exec_dup (boolean_not float_cbrt float_cbrt in1 in1 in1 boolean_and float_positive) float_- float_- in1 false float_* exec_dup (float_- exec_if () ()))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 37
;;; -------------------------------------------------------
;;; Best plushy: (float_* boolean_or boolean_and in1 in1 float_cbrt float_sqrt boolean_and in1 boolean_or float_* float_* boolean_or in1 exec_if float_* float_cbrt float_cbrt float_cbrt float_+)
;;; Best program: (float_* boolean_or boolean_and in1 in1 float_cbrt float_sqrt boolean_and in1 boolean_or float_* float_* boolean_or in1 exec_if (float_* float_cbrt float_cbrt float_cbrt float_+) ())
;;; Best total error: 10935.240661621094
;;; Best errors: (143.7537078857422 142.46074676513672 103.5482177734375 158.96929931640625 141.0594024658203 124.25350189208984 139.14359664916992 92.71305847167969 47.754451751708984 23.606678009033203 158.57942962646484 92.73605346679688 111.80570983886719 157.82990264892578 115.04093551635742 69.81361389160156 118.96796417236328 117.04230499267578 0.35898590087890625 145.50654220581055 45.51628112792969 129.76988983154297 156.57942962646484 111.1509017944336 85.4786148071289 29.318466186523438 109.71601104736328 45.79435348510742 140.50654220581055 149.93174362182617 93.4240493774414 150.3965835571289 135.08324432373047 140.7130584716797 150.41032028198242 97.30015563964844 141.4240493774414 157.98926544189453 139.96796417236328 85.80570983886719 140.7353286743164 116.50975036621094 25.56555938720703 159.8582534790039 125.84122848510742 141.37655639648438 76.63832473754883 124.95663833618164 115.04093551635742 157.2516860961914 145.56147003173828 116.09585571289062 102.61513137817383 21.42789077758789 163.68590545654297 143.05083847045898 125.38940048217773 144.63921356201172 155.3888397216797 106.36391830444336 119.30143737792969 94.0268669128418 27.287349700927734 93.37655639648438 144.50975036621094 112.04230499267578 8.657215118408203 22.52530288696289 157.71601104736328 147.64987182617188 116.4240493774414 45.79435348510742 102.04093551635742 143.8458023071289 47.51628112792969 149.9490737915039 158.3714141845703 21.029132843017578 119.35508728027344 105.05083847045898 159.98926544189453 44.6572151184082 162.82990264892578 150.84122848510742 149.9490737915039 140.50654220581055 141.7537078857422 150.61513137817383 115.60465621948242 66.04230499267578 158.57942962646484 83.68609619140625 107.56147003173828 143.8458023071289 48.43444061279297 71.95072555541992 27.230121612548828 45.79435348510742 119.68609619140625)
;;; Best behaviors: (-51.753708 -54.460747 -61.548218 -68.9693 -51.059402 -59.2535 -49.143597 -50.71306 42.24555 41.393322 -68.57943 -50.736053 -69.80571 -67.8299 -50.040936 -53.813614 -51.967964 -50.042305 42.358986 -50.506542 42.48372 -67.76989 -68.57943 -69.1509 -69.478615 44.318466 -67.71601 44.205647 -50.506542 -59.931744 -51.42405 -60.396584 -70.083244 -50.71306 -60.41032 -55.300156 -51.42405 -69.989265 -51.967964 -69.80571 -50.73533 -54.50975 41.56556 -69.85825 -60.84123 -51.376556 -60.638325 -59.95664 -50.040936 -69.251686 -55.56147 -51.095856 -60.61513 43.57211 -68.685905 -53.05084 -60.3894 -54.639214 -67.38884 -54.36392 -54.301437 -52.026867 43.28735 -51.376556 -54.50975 -50.042305 43.342785 42.474697 -67.71601 -55.64987 -51.42405 44.205647 -50.040936 -53.845802 42.48372 -59.949074 -68.371414 43.970867 -54.355087 -53.05084 -69.989265 43.342785 -67.8299 -60.84123 -59.949074 -50.506542 -51.753708 -60.61513 -50.604656 -50.042305 -68.57943 -67.6861 -55.56147 -53.845802 41.56556 -55.950726 43.23012 44.205647 -67.6861)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 38
;;; -------------------------------------------------------
;;; Best plushy: (pi float_* float_% float_positive pi in1 in1 boolean_and boolean_and false false float_cbrt float_* float_+ exec_dup float_* boolean_= exec_dup float_+ float_* exec_if)
;;; Best program: (pi float_* float_% float_positive pi in1 in1 boolean_and boolean_and false false float_cbrt float_* float_+ exec_dup (float_* boolean_= exec_dup (float_+ float_* exec_if () ())))
;;; Best total error: 10008.611893773079
;;; Best errors: (153.9438362121582 115.78440475463867 48.57959318161011 135.58665466308594 153.82365798950195 70.37937879562378 154.76085662841797 105.0726203918457 86.56616592407227 63.05992805957794 136.37508010864258 104.32040023803711 86.20153045654297 136.7684783935547 128.2604866027832 44.63098907470703 129.69665908813477 130.0726203918457 40.358489990234375 158.63600540161133 85.16389489173889 109.16134262084961 134.37508010864258 87.19161224365234 61.783966064453125 11.715631008148193 88.37508010864258 86.41667795181274 153.63600540161133 95.97915697097778 103.9438362121582 94.78019189834595 109.59799194335938 153.0726203918457 95.679190158844 70.29154968261719 151.9438362121582 133.19161224365234 150.69665908813477 60.20153045654297 152.5085678100586 90.12223434448242 13.761313915252686 135.19161224365234 69.92993688583374 152.69665908813477 21.829153060913086 70.07971620559692 128.2604866027832 133.58665466308594 117.95318794250488 128.63600540161133 46.78019189834595 61.26716589927673 139.99388122558594 117.61588287353516 71.279287815094 117.95318794250488 134.7684783935547 78.27681159973145 90.9444808959961 104.32040023803711 13.014493942260742 104.69665908813477 118.12223434448242 125.0726203918457 48.416677951812744 61.416677951812744 136.37508010864258 120.63098907470703 126.9438362121582 86.41667795181274 115.2604866027832 117.78440475463867 87.16389489173889 94.63047885894775 136.7684783935547 60.818471908569336 92.11186027526855 79.61588287353516 135.19161224365234 84.41667795181274 141.7684783935547 94.92993688583374 94.63047885894775 153.63600540161133 151.9438362121582 94.78019189834595 129.01123809814453 79.0726203918457 136.37508010864258 62.76847839355469 79.95318794250488 117.78440475463867 87.76131391525269 43.44761848449707 12.865072965621948 86.41667795181274 98.76847839355469)
;;; Best behaviors: (-61.943836 -27.784405 -6.579593 -45.586655 -63.823658 -5.379379 -64.76086 -63.07262 3.433834 1.940072 -46.37508 -62.3204 -44.20153 -46.76848 -63.260487 -28.63099 -62.69666 -63.07262 1.64151 -63.636005 2.836105 -47.161343 -46.37508 -45.191612 -45.783966 3.284369 -46.37508 3.583322 -63.636005 -5.979157 -61.943836 -4.780192 -44.597992 -63.07262 -5.67919 -28.29155 -61.943836 -45.191612 -62.69666 -44.20153 -62.508568 -28.122234 2.238686 -45.191612 -4.929937 -62.69666 -5.829153 -5.079716 -63.260487 -45.586655 -27.953188 -63.636005 -4.780192 3.732834 -44.99388 -27.615883 -6.279288 -27.953188 -46.76848 -26.276812 -25.94448 -62.3204 2.985506 -62.69666 -28.122234 -63.07262 3.583322 3.583322 -46.37508 -28.63099 -61.943836 3.583322 -63.260487 -27.784405 2.836105 -4.630479 -46.76848 4.181528 -27.11186 -27.615883 -45.191612 3.583322 -46.76848 -4.929937 -4.630479 -63.636005 -61.943836 -4.780192 -64.01124 -63.07262 -46.37508 -46.76848 -27.953188 -27.784405 2.238686 -27.447618 3.134927 3.583322 -46.76848)
;;; 
;;; 
;; <-

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@
