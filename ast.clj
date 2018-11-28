;; gorilla-repl.fileformat = 1

;; @@
(comment
  1. set input
  2. set output
  3. lexicase selection cases)

(ns ast
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

;;(defn get-input
;;  [filename]
;;  (rest (map #(map to-float (drop-last (read-row filename %))) 
;;             (range (count (read-column filename 0))))))
;;(defn get-target
;;  [file-name]
;;  (doall
;;    (map #(float (read-string %))
;;         (rest (read-column file-name 11)))))

(defn get-input
  [filename]
  (rest (map #(map to-float (drop-last (read-row filename %))) 
             (range 20))))

(defn get-target
  [file-name]
  (doall
    (map to-float
         (rest (take 21 (read-column file-name 11))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/get-target</span>","value":"#'ast/get-target"}
;; <=

;; @@

(def input (get-input data-addr))

(def target (get-target data-addr))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/target</span>","value":"#'ast/target"}
;; <=

;; @@
input
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>((615.0 349.04605 -61.943836 320.79654 -51.753708 1.0 0.0 0.0 0.0 0.0 0.017) (713.0 53.085938 -27.784405 223.52551 -54.460747 1.0 1.8181 1.6267 0.2552 45.4063 0.007) (730.0 33.57422 -6.579593 170.45558 -61.548218 1.0 0.232 0.2262 0.0157 40.2561 0.021) (745.0 0.189873 -45.586655 328.25446 -68.9693 1.0 0.3037 0.2813 1.1523 40.7951 0.007) (1124.0 352.71127 -63.823658 316.9223 -51.059402 1.0 0.1934 0.2415 0.0176 40.4166 0.024) (1227.0 35.683594 -5.379379 171.99295 -59.2535 1.0 0.0 0.0 0.0 0.0 0.02) (1598.0 347.8467 -64.76086 318.92984 -49.143597 1.0 0.1352 0.182 0.0304 39.7279 0.019) (1632.0 348.5959 -63.07262 320.0233 -50.71306 1.0 0.6857 0.7014 0.01 43.1524 0.021) (1920.0 149.41406 3.433834 234.91913 42.24555 1.0 0.3088 0.3229 0.336 41.1401 0.027) (1926.0 149.41406 1.940072 236.56537 41.393322 1.0 0.0 0.0 0.0 0.0 0.018) (2072.0 0.965665 -46.37508 325.84592 -68.57943 1.0 0.1516 0.19 0.0104 39.8317 0.007) (2103.0 346.5 -62.3204 321.95114 -50.736053 1.0 0.1695 0.5409 0.2283 42.4667 0.02) (2300.0 359.44672 -44.20153 331.73 -69.80571 1.0 0.236 2.7474 0.5335 46.7959 0.01) (2330.0 359.8052 -46.76848 327.136 -67.8299 1.0 0.4541 0.5736 0.2827 42.6207 0.011) (2624.0 346.65518 -63.260487 320.9522 -50.040936 1.0 0.0 0.0 0.0 0.0 0.019) (2677.0 53.964844 -28.63099 225.14294 -53.813614 1.0 0.0 0.0 0.0 0.0 0.009) (2922.0 352.39865 -62.69666 318.01743 -51.967964 1.0 0.1539 0.1469 0.0094 39.2171 0.02) (3041.0 346.13013 -63.07262 321.4231 -50.042305 1.0 0.1069 0.1274 0.0198 38.88 0.02) (3285.0 150.82031 1.64151 237.9945 42.358986 1.0 0.161 0.1818 0.0079 39.7258 0.02))</span>","value":"((615.0 349.04605 -61.943836 320.79654 -51.753708 1.0 0.0 0.0 0.0 0.0 0.017) (713.0 53.085938 -27.784405 223.52551 -54.460747 1.0 1.8181 1.6267 0.2552 45.4063 0.007) (730.0 33.57422 -6.579593 170.45558 -61.548218 1.0 0.232 0.2262 0.0157 40.2561 0.021) (745.0 0.189873 -45.586655 328.25446 -68.9693 1.0 0.3037 0.2813 1.1523 40.7951 0.007) (1124.0 352.71127 -63.823658 316.9223 -51.059402 1.0 0.1934 0.2415 0.0176 40.4166 0.024) (1227.0 35.683594 -5.379379 171.99295 -59.2535 1.0 0.0 0.0 0.0 0.0 0.02) (1598.0 347.8467 -64.76086 318.92984 -49.143597 1.0 0.1352 0.182 0.0304 39.7279 0.019) (1632.0 348.5959 -63.07262 320.0233 -50.71306 1.0 0.6857 0.7014 0.01 43.1524 0.021) (1920.0 149.41406 3.433834 234.91913 42.24555 1.0 0.3088 0.3229 0.336 41.1401 0.027) (1926.0 149.41406 1.940072 236.56537 41.393322 1.0 0.0 0.0 0.0 0.0 0.018) (2072.0 0.965665 -46.37508 325.84592 -68.57943 1.0 0.1516 0.19 0.0104 39.8317 0.007) (2103.0 346.5 -62.3204 321.95114 -50.736053 1.0 0.1695 0.5409 0.2283 42.4667 0.02) (2300.0 359.44672 -44.20153 331.73 -69.80571 1.0 0.236 2.7474 0.5335 46.7959 0.01) (2330.0 359.8052 -46.76848 327.136 -67.8299 1.0 0.4541 0.5736 0.2827 42.6207 0.011) (2624.0 346.65518 -63.260487 320.9522 -50.040936 1.0 0.0 0.0 0.0 0.0 0.019) (2677.0 53.964844 -28.63099 225.14294 -53.813614 1.0 0.0 0.0 0.0 0.0 0.009) (2922.0 352.39865 -62.69666 318.01743 -51.967964 1.0 0.1539 0.1469 0.0094 39.2171 0.02) (3041.0 346.13013 -63.07262 321.4231 -50.042305 1.0 0.1069 0.1274 0.0198 38.88 0.02) (3285.0 150.82031 1.64151 237.9945 42.358986 1.0 0.161 0.1818 0.0079 39.7258 0.02))"}
;; <=

;; @@
target
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>92.0</span>","value":"92.0"},{"type":"html","content":"<span class='clj-unkown'>88.0</span>","value":"88.0"},{"type":"html","content":"<span class='clj-unkown'>42.0</span>","value":"42.0"},{"type":"html","content":"<span class='clj-unkown'>90.0</span>","value":"90.0"},{"type":"html","content":"<span class='clj-unkown'>90.0</span>","value":"90.0"},{"type":"html","content":"<span class='clj-unkown'>65.0</span>","value":"65.0"},{"type":"html","content":"<span class='clj-unkown'>90.0</span>","value":"90.0"},{"type":"html","content":"<span class='clj-unkown'>42.0</span>","value":"42.0"},{"type":"html","content":"<span class='clj-unkown'>90.0</span>","value":"90.0"},{"type":"html","content":"<span class='clj-unkown'>65.0</span>","value":"65.0"},{"type":"html","content":"<span class='clj-unkown'>90.0</span>","value":"90.0"},{"type":"html","content":"<span class='clj-unkown'>42.0</span>","value":"42.0"},{"type":"html","content":"<span class='clj-unkown'>42.0</span>","value":"42.0"},{"type":"html","content":"<span class='clj-unkown'>90.0</span>","value":"90.0"},{"type":"html","content":"<span class='clj-unkown'>65.0</span>","value":"65.0"},{"type":"html","content":"<span class='clj-unkown'>16.0</span>","value":"16.0"},{"type":"html","content":"<span class='clj-unkown'>67.0</span>","value":"67.0"},{"type":"html","content":"<span class='clj-unkown'>67.0</span>","value":"67.0"},{"type":"html","content":"<span class='clj-unkown'>42.0</span>","value":"42.0"},{"type":"html","content":"<span class='clj-unkown'>95.0</span>","value":"95.0"}],"value":"(92.0 88.0 42.0 90.0 90.0 65.0 90.0 42.0 90.0 65.0 90.0 42.0 42.0 90.0 65.0 16.0 67.0 67.0 42.0 95.0)"}
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
    'boolean_negative
    'boolean_positive
    ;'float_absolute
   ; 'float_sqrt
   ; 'float_cbrt
    ;'float_+
  ;  'float_-
   ; 'float_*
    ;;'float_%
  ;  'float_=
   ))


(def example-push-state
  {:exec '()
   :float '(1.0 2.0 3.0 4.0 5.0 6.0 7.0)
   :input {:in1 4}})


(def opens ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1 
   'exec_if 2
   'float_+ 1})


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/opens</span>","value":"#'ast/opens"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/make-push-instruction</span>","value":"#'ast/make-push-instruction"}
;; <=

;; @@


;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in1 (:input state))))


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
                         #(Math/abs %)
                         [:float]
                         :float))

(defn boolean_negative
  [state]
  (make-push-instruction state
                         #(neg? %)
                         [:float]
                         :boolean))


(defn boolean_positive
  [state]
  (make-push-instruction state
                         #(pos? %)
                         [:float]
                         :boolean))

(defn float_sqrt
  [state]
  (make-push-instruction state
                         #(Math/sqrt %)
                         [:float]
                         :float))

(defn float_cbrt
  [state]
  (make-push-instruction state
                         #(Math/cbrt %)
                         [:float]
                         [:float]))



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/float_cbrt</span>","value":"#'ast/float_cbrt"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/push-from-plushy</span>","value":"#'ast/push-from-plushy"}
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
  [plushy instructions mutation-rate]
  (map #(if (<= (rand) mutation-rate)
             (vector (rand-nth instructions))
             %) 
          plushy))

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
       (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
                               (:plushy (select-parent pop argmap))
                               argmap)
                    
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap)
                                       (:mutation-rate argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap))
                               (:mutation-rate argmap))))})



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/new-individual</span>","value":"#'ast/new-individual"}
;; <=

;; @@
;;------------------------------------------------------------------------------------------
(defn report
  "Reports information each generation."
  [pop generation]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
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
                                                    max-initial-plushy-size)))]
    (let [evaluated-pop (sort-by :total-error 
                                 (map (partial error-function argmap)
                                      population))]
      (report evaluated-pop generation)
      (cond
        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size #(new-individual evaluated-pop argmap)))))))


(defn regression-error-function
  "Finds the behaviors and errors of the individual."
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
        
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (abs (- correct-output output))))
                    correct-outputs
                    outputs)]
    
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply +' errors))))



(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'ast)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :error-function regression-error-function
                                  :max-generations 500
                                  :population-size 200
                                  :max-initial-plushy-size 50
                                  :step-limit 100
                                  :parent-selection :lexicase
                                  :tournament-size 5
                                  :mutation-rate 0.1
                                  :crossover :uniform-crossover
                                  :bit-mutation true}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;ast/-main</span>","value":"#'ast/-main"}
;; <=

;; @@
(-main)
;; @@
;; ->
;;; Starting GP with args: {:max-initial-plushy-size 50, :bit-mutation true, :crossover :uniform-crossover, :mutation-rate 0.1, :instructions (in1 exec_dup exec_if boolean_and boolean_or boolean_not boolean_= close true false pi e boolean_negative boolean_positive), :max-generations 500, :parent-selection :lexicase, :tournament-size 5, :step-limit 100, :error-function #function[ast/regression-error-function], :population-size 200}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (true boolean_negative exec_if false boolean_or false boolean_negative exec_if boolean_= boolean_and boolean_= boolean_or boolean_or boolean_and boolean_or boolean_= close boolean_and boolean_= true boolean_or true boolean_positive boolean_negative true e boolean_= boolean_not boolean_positive boolean_= boolean_= boolean_= exec_dup true boolean_positive in1 exec_if boolean_positive)
;;; Best program: (true boolean_negative exec_if (false boolean_or false boolean_negative exec_if (boolean_= boolean_and boolean_= boolean_or boolean_or boolean_and boolean_or boolean_=) (boolean_and boolean_= true boolean_or true boolean_positive boolean_negative true e boolean_= boolean_not boolean_positive boolean_= boolean_= boolean_= exec_dup (true boolean_positive in1 exec_if (boolean_positive) ()))) ())
;;; Best total error: 707.3146020825952
;;; Best errors: (91.98299999907613 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 64.98000000044703 50.272098541259766 1.1524009704589844 48.859901428222656 64.98200000077486 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 64.98100000061095 15.99100000038743 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (0.017 45.4063 40.2561 40.7951 40.4166 0.02 39.7279 43.1524 41.1401 0.018 39.8317 42.4667 46.7959 42.6207 0.019 0.009 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: (true boolean_negative exec_if false boolean_or false boolean_negative exec_if boolean_= boolean_and boolean_= boolean_or boolean_or boolean_and boolean_or boolean_= close boolean_and boolean_= true boolean_or true boolean_positive boolean_negative true e boolean_= boolean_not boolean_positive boolean_= boolean_= boolean_= exec_dup true boolean_positive in1 exec_if boolean_positive)
;;; Best program: (true boolean_negative exec_if (false boolean_or false boolean_negative exec_if (boolean_= boolean_and boolean_= boolean_or boolean_or boolean_and boolean_or boolean_=) (boolean_and boolean_= true boolean_or true boolean_positive boolean_negative true e boolean_= boolean_not boolean_positive boolean_= boolean_= boolean_= exec_dup (true boolean_positive in1 exec_if (boolean_positive) ()))) ())
;;; Best total error: 707.3146020825952
;;; Best errors: (91.98299999907613 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 64.98000000044703 50.272098541259766 1.1524009704589844 48.859901428222656 64.98200000077486 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 64.98100000061095 15.99100000038743 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (0.017 45.4063 40.2561 40.7951 40.4166 0.02 39.7279 43.1524 41.1401 0.018 39.8317 42.4667 46.7959 42.6207 0.019 0.009 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: (true boolean_negative exec_if false boolean_or false exec_if boolean_and boolean_= boolean_or boolean_or boolean_and boolean_or boolean_= close boolean_and boolean_and true boolean_or true boolean_positive boolean_negative true e boolean_= boolean_not boolean_positive in1 boolean_= boolean_= boolean_= exec_dup boolean_not true boolean_positive in1 exec_if boolean_positive)
;;; Best program: (true boolean_negative exec_if (false boolean_or false exec_if (boolean_and boolean_= boolean_or boolean_or boolean_and boolean_or boolean_=) (boolean_and boolean_and true boolean_or true boolean_positive boolean_negative true e boolean_= boolean_not boolean_positive in1 boolean_= boolean_= boolean_= exec_dup (boolean_not true boolean_positive in1 exec_if (boolean_positive) ()))) ())
;;; Best total error: 707.3146020825952
;;; Best errors: (91.98299999907613 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 64.98000000044703 50.272098541259766 1.1524009704589844 48.859901428222656 64.98200000077486 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 64.98100000061095 15.99100000038743 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (0.017 45.4063 40.2561 40.7951 40.4166 0.02 39.7279 43.1524 41.1401 0.018 39.8317 42.4667 46.7959 42.6207 0.019 0.009 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: (true exec_if false boolean_or false boolean_= boolean_and boolean_= boolean_or boolean_or boolean_and boolean_or boolean_= boolean_and boolean_= true boolean_or true boolean_positive e boolean_= boolean_not boolean_positive boolean_= boolean_= boolean_= exec_dup true boolean_positive in1 exec_if boolean_positive)
;;; Best program: (true exec_if (false boolean_or false boolean_= boolean_and boolean_= boolean_or boolean_or boolean_and boolean_or boolean_= boolean_and boolean_= true boolean_or true boolean_positive e boolean_= boolean_not boolean_positive boolean_= boolean_= boolean_= exec_dup (true boolean_positive in1 exec_if (boolean_positive) ())) ())
;;; Best total error: 707.3146020825952
;;; Best errors: (91.98299999907613 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 64.98000000044703 50.272098541259766 1.1524009704589844 48.859901428222656 64.98200000077486 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 64.98100000061095 15.99100000038743 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (0.017 45.4063 40.2561 40.7951 40.4166 0.02 39.7279 43.1524 41.1401 0.018 39.8317 42.4667 46.7959 42.6207 0.019 0.009 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: (true exec_if false boolean_or boolean_positive false exec_if boolean_= boolean_and boolean_= boolean_or boolean_or in1 boolean_and boolean_or boolean_= close boolean_and boolean_and true boolean_or boolean_positive boolean_negative true e boolean_negative boolean_= boolean_not boolean_positive boolean_= boolean_= exec_dup true boolean_positive in1 exec_if boolean_positive)
;;; Best program: (true exec_if (false boolean_or boolean_positive false exec_if (boolean_= boolean_and boolean_= boolean_or boolean_or in1 boolean_and boolean_or boolean_=) (boolean_and boolean_and true boolean_or boolean_positive boolean_negative true e boolean_negative boolean_= boolean_not boolean_positive boolean_= boolean_= exec_dup (true boolean_positive in1 exec_if (boolean_positive) ()))) ())
;;; Best total error: 707.3146020825952
;;; Best errors: (91.98299999907613 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 64.98000000044703 50.272098541259766 1.1524009704589844 48.859901428222656 64.98200000077486 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 64.98100000061095 15.99100000038743 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (0.017 45.4063 40.2561 40.7951 40.4166 0.02 39.7279 43.1524 41.1401 0.018 39.8317 42.4667 46.7959 42.6207 0.019 0.009 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: (true boolean_negative boolean_= exec_if false boolean_negative boolean_or false boolean_negative exec_if boolean_and boolean_= boolean_and boolean_= boolean_= boolean_or boolean_or boolean_and boolean_or boolean_= close boolean_and exec_if boolean_= true close boolean_or true boolean_positive boolean_negative true e boolean_= boolean_not boolean_positive boolean_= boolean_= exec_dup true boolean_positive in1 exec_if boolean_positive)
;;; Best program: (true boolean_negative boolean_= exec_if (false boolean_negative boolean_or false boolean_negative exec_if (boolean_and boolean_= boolean_and boolean_= boolean_= boolean_or boolean_or boolean_and boolean_or boolean_=) (boolean_and exec_if (boolean_= true) (boolean_or true boolean_positive boolean_negative true e boolean_= boolean_not boolean_positive boolean_= boolean_= exec_dup (true boolean_positive in1 exec_if (boolean_positive) ())))) ())
;;; Best total error: 707.3146020825952
;;; Best errors: (91.98299999907613 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 64.98000000044703 50.272098541259766 1.1524009704589844 48.859901428222656 64.98200000077486 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 64.98100000061095 15.99100000038743 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (0.017 45.4063 40.2561 40.7951 40.4166 0.02 39.7279 43.1524 41.1401 0.018 39.8317 42.4667 46.7959 42.6207 0.019 0.009 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: (true true exec_if boolean_or close boolean_or boolean_positive boolean_= exec_if boolean_positive boolean_and boolean_= close boolean_or boolean_or boolean_positive close boolean_= boolean_or close boolean_not true exec_dup false boolean_or boolean_positive boolean_negative e boolean_positive boolean_= boolean_positive boolean_positive exec_if boolean_= exec_dup in1 boolean_positive)
;;; Best program: (true true exec_if (boolean_or) (boolean_or boolean_positive boolean_= exec_if (boolean_positive boolean_and boolean_=) (boolean_or boolean_or boolean_positive) boolean_= boolean_or) boolean_not true exec_dup (false boolean_or boolean_positive boolean_negative e boolean_positive boolean_= boolean_positive boolean_positive exec_if (boolean_= exec_dup (in1 boolean_positive)) ()))
;;; Best total error: 702.3976020812988
;;; Best errors: (91.0 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 64.0 50.272098541259766 1.1524009704589844 48.859901428222656 64.0 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 64.0 15.0 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (1.0 45.4063 40.2561 40.7951 40.4166 1.0 39.7279 43.1524 41.1401 1.0 39.8317 42.4667 46.7959 42.6207 1.0 1.0 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 7
;;; -------------------------------------------------------
;;; Best plushy: (true true exec_if boolean_or close boolean_or boolean_positive boolean_= exec_if boolean_positive boolean_and boolean_= close boolean_or boolean_or boolean_positive close boolean_= boolean_or close boolean_not true exec_dup false boolean_or boolean_positive boolean_negative e boolean_positive boolean_= boolean_positive boolean_positive exec_if boolean_= exec_dup in1 boolean_positive)
;;; Best program: (true true exec_if (boolean_or) (boolean_or boolean_positive boolean_= exec_if (boolean_positive boolean_and boolean_=) (boolean_or boolean_or boolean_positive) boolean_= boolean_or) boolean_not true exec_dup (false boolean_or boolean_positive boolean_negative e boolean_positive boolean_= boolean_positive boolean_positive exec_if (boolean_= exec_dup (in1 boolean_positive)) ()))
;;; Best total error: 702.3976020812988
;;; Best errors: (91.0 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 64.0 50.272098541259766 1.1524009704589844 48.859901428222656 64.0 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 64.0 15.0 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (1.0 45.4063 40.2561 40.7951 40.4166 1.0 39.7279 43.1524 41.1401 1.0 39.8317 42.4667 46.7959 42.6207 1.0 1.0 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; -------------------------------------------------------
;;; Best plushy: (true close boolean_positive e false boolean_positive boolean_positive close boolean_= e boolean_= in1 boolean_and boolean_= boolean_or pi in1 close boolean_= boolean_positive close boolean_not boolean_or true true e boolean_positive true exec_dup boolean_negative e boolean_and boolean_positive false boolean_not false boolean_not boolean_= boolean_positive boolean_= boolean_or boolean_= exec_dup boolean_positive exec_if boolean_positive)
;;; Best program: (true boolean_positive e false boolean_positive boolean_positive boolean_= e boolean_= in1 boolean_and boolean_= boolean_or pi in1 boolean_= boolean_positive boolean_not boolean_or true true e boolean_positive true exec_dup (boolean_negative e boolean_and boolean_positive false boolean_not false boolean_not boolean_= boolean_positive boolean_= boolean_or boolean_= exec_dup (boolean_positive exec_if (boolean_positive) ())))
;;; Best total error: 691.6896388133499
;;; Best errors: (88.8584073464102 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 61.8584073464102 50.272098541259766 1.1524009704589844 48.859901428222656 61.8584073464102 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 61.8584073464102 12.858407346410207 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (3.141592653589793 45.4063 40.2561 40.7951 40.4166 3.141592653589793 39.7279 43.1524 41.1401 3.141592653589793 39.8317 42.4667 46.7959 42.6207 3.141592653589793 3.141592653589793 39.2171 38.88 39.7258)
;;; 
;;; 
;; <-

;; @@

;; @@
