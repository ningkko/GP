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
          (rand-nth instructions)
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
       (< prob 0.5) (let[crossed-plushy (crossover 
                                         (:plushy (select-parent pop argmap)) 
                                         (:plushy (select-parent pop argmap))
                                         argmap)]
                      (if (:bit-mutation argmap)
                        (bit-mutation crossed-plushy (:instructions argmap) (:mutation-rate argmap))
                        crossed-plushy))
       
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
           :total-error (let [total-err (apply +' errors)]
                          (if (Double/isNaN total-err)
                            2000000
                            total-err)))))



(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'ast)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :error-function regression-error-function
                                  :max-generations 30
                                  :population-size 200
                                  :max-initial-plushy-size 30
                                  :step-limit 10
                                  :parent-selection :lexicase
                                  :tournament-size 5
                                  :mutation-rate 1
                                  :crossover :uniform-crossover
                                  :bit-mutation true}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))


;; @@

;; @@
(-main)
;; @@
;; ->
;;; Starting GP with args: {:max-initial-plushy-size 50, :bit-mutation true, :crossover :uniform-crossover, :mutation-rate 1, :instructions (in1 exec_dup exec_if boolean_and boolean_or boolean_not boolean_= close true false pi e float_negative float_positive float_absolute float_sqrt float_+ float_- float_* float_=), :max-generations 30, :parent-selection :lexicase, :tournament-size 5, :step-limit 100, :error-function #function[ast/regression-error-function], :population-size 200}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (in1 exec_if float_+ true float_- false in1 float_* exec_dup close exec_dup boolean_and pi float_- float_sqrt close float_negative float_sqrt pi true float_- boolean_not in1 boolean_= float_- pi exec_dup float_+ float_- float_positive boolean_and float_absolute)
;;; Best program: (in1 exec_if (float_+ (true float_- false in1 float_* exec_dup () exec_dup (boolean_and pi float_- float_sqrt) float_negative float_sqrt pi true float_- boolean_not in1 boolean_= float_- pi exec_dup (float_+ (float_- float_positive boolean_and float_absolute)))) ())
;;; Best total error: 445.23597717285156
;;; Best errors: (40.24629211425781 33.53925323486328 19.5482177734375 21.03070068359375 38.94059753417969 5.746498107910156 40.85640335083008 8.713058471679688 47.754451751708984 23.606678009033203 21.420570373535156 8.736053466796875 27.805709838867188 22.17009735107422 14.959064483642578 37.81361389160156 15.032035827636719 16.95769500732422 0.35898590087890625)
;;; Best behaviors: (51.753708 54.460747 61.548218 68.9693 51.059402 59.2535 49.143597 50.71306 42.24555 41.393322 68.57943 50.736053 69.80571 67.8299 50.040936 53.813614 51.967964 50.042305 42.358986)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: (float_positive float_negative e exec_if true boolean_= in1 float_- false float_- e exec_dup false float_absolute float_negative false exec_dup e float_positive float_positive boolean_not float_+ float_+ boolean_and e float_+ boolean_and in1 boolean_or in1 boolean_and e boolean_not true boolean_or close boolean_and exec_dup e float_positive exec_dup)
;;; Best program: (float_positive float_negative e exec_if (true boolean_= in1 float_- false float_- e exec_dup (false float_absolute float_negative false exec_dup (e float_positive float_positive boolean_not float_+ (float_+ (boolean_and e float_+ (boolean_and in1 boolean_or in1 boolean_and e boolean_not true boolean_or) boolean_and exec_dup (e float_positive exec_dup ())))))) ())
;;; Best total error: 705.8170021073893
;;; Best errors: (91.98299999907613 42.33150041010231 1.707200299948454 48.045598833821714 49.54180072620511 64.98000000044703 50.22269854135811 1.183400969952345 48.496901432052255 64.98200000077486 50.15090062841773 0.7150015123188496 5.339398453012109 47.08559916168451 64.98100000061095 15.99100000038743 27.75350176449865 28.08019893243909 2.2463004402816296)
;;; Best behaviors: (0.017000000923871994 45.66849958989769 40.292799700051546 41.954401166178286 40.45819927379489 0.019999999552965164 39.77730145864189 43.183400969952345 41.503098567947745 0.017999999225139618 39.84909937158227 42.71500151231885 47.33939845301211 42.91440083831549 0.01899999938905239 0.008999999612569809 39.24649823550135 38.91980106756091 39.75369955971837)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or boolean_or e boolean_not float_+ float_+ float_* boolean_and float_* pi float_+ pi exec_dup float_negative float_negative float_positive e boolean_or float_positive float_* exec_if true float_= in1 boolean_= float_sqrt pi exec_dup exec_if float_- float_- float_+)
;;; Best program: (boolean_or boolean_or e boolean_not float_+ (float_+ (float_* boolean_and float_* pi float_+ (pi exec_dup (float_negative float_negative float_positive e boolean_or float_positive float_* exec_if (true float_= in1 boolean_= float_sqrt pi exec_dup (exec_if (float_- float_- float_+ ()) ())) ())))))
;;; Best total error: 670.19179470914
;;; Best errors: (88.8584073464102 39.28057376096909 1.2684785911166188 44.9946721846885 46.57912741403048 61.8584073464102 47.23794636676972 4.159079861017418 45.546625552577424 61.8584073464102 47.09997397928451 3.6951728184991595 8.370991105335307 44.05988739232485 61.8584073464102 12.858407346410207 24.77333045831834 25.10002762625878 0.7338708658986803)
;;; Best behaviors: (3.141592653589793 48.71942623903091 43.26847859111662 45.0053278153115 43.42087258596952 3.141592653589793 42.76205363323028 46.15907986101742 44.453374447422576 3.141592653589793 42.90002602071549 45.69517281849916 50.37099110533531 45.94011260767515 3.141592653589793 3.141592653589793 42.22666954168166 41.89997237374122 42.73387086589868)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or float_negative float_negative float_negative boolean_= in1 true float_negative float_* float_negative float_negative false exec_if boolean_not true float_absolute close boolean_or float_positive float_+ float_absolute float_+ exec_dup in1 e float_negative in1 float_sqrt false float_negative boolean_not float_* e exec_dup boolean_and boolean_and float_= boolean_= e false close float_sqrt float_= float_absolute boolean_and boolean_and boolean_or float_sqrt float_positive float_+ exec_dup close float_negative boolean_= float_absolute float_absolute boolean_and exec_if boolean_= pi in1 true boolean_not float_sqrt float_+)
;;; Best program: (boolean_or float_negative float_negative float_negative boolean_= in1 true float_negative float_* float_negative float_negative false exec_if (boolean_not true float_absolute) (boolean_or float_positive float_+ (float_absolute float_+ (exec_dup (in1 e float_negative in1 float_sqrt false float_negative boolean_not float_* e exec_dup (boolean_and boolean_and float_= boolean_= e false) float_sqrt float_= float_absolute boolean_and boolean_and boolean_or float_sqrt float_positive float_+ (exec_dup () float_negative boolean_= float_absolute float_absolute boolean_and exec_if (boolean_= pi in1 true boolean_not float_sqrt float_+ ()) ()))))))
;;; Best total error: 624.1784173250198
;;; Best errors: (30.056163787841797 60.21559524536133 35.42040681838989 44.41334533691406 26.176342010498047 59.62062120437622 25.23914337158203 21.072620391845703 86.56616592407227 63.05992805957794 43.62491989135742 20.32040023803711 2.2015304565429688 43.23152160644531 1.7395133972167969 12.630989074707031 4.303340911865234 3.927379608154297 40.358489990234375)
;;; Best behaviors: (61.943836 27.784405 6.579593 45.586655 63.823658 5.379379 64.76086 63.07262 3.433834 1.940072 46.37508 62.3204 44.20153 46.76848 63.260487 28.63099 62.69666 63.07262 1.64151)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: (float_+ exec_if boolean_= true float_positive in1 true exec_dup e float_- float_absolute close false true true float_- exec_dup float_= pi false float_sqrt true boolean_or e float_= false e float_sqrt true float_* boolean_and pi float_- true float_= float_+ float_* float_sqrt float_sqrt exec_dup float_+ float_* exec_if pi true true float_absolute true boolean_or float_negative float_absolute close true boolean_or boolean_not e in1 float_negative boolean_= float_negative boolean_not float_- boolean_= exec_dup exec_dup float_- float_+ float_absolute float_positive float_absolute boolean_not float_negative in1 float_+ boolean_and float_* float_negative boolean_or boolean_= close float_* boolean_or boolean_and float_- float_- float_sqrt e true float_- boolean_not float_absolute float_positive float_positive true float_negative float_negative exec_if boolean_= exec_dup float_= float_* boolean_or boolean_or true boolean_and e float_= exec_dup exec_if float_negative boolean_and false close boolean_or float_absolute true float_negative float_= true boolean_not float_negative pi exec_dup pi false boolean_= pi pi boolean_and boolean_= e)
;;; Best program: (float_+ (exec_if (boolean_= true float_positive in1 true exec_dup (e float_- float_absolute) false true true float_- exec_dup (float_= pi false float_sqrt true boolean_or e float_= false e float_sqrt true float_* boolean_and pi float_- true float_= float_+ (float_* float_sqrt float_sqrt exec_dup (float_+ (float_* exec_if (pi true true float_absolute true boolean_or float_negative float_absolute) (true boolean_or boolean_not e in1 float_negative boolean_= float_negative boolean_not float_- boolean_= exec_dup (exec_dup (float_- float_+ (float_absolute float_positive float_absolute boolean_not float_negative in1 float_+ (boolean_and float_* float_negative boolean_or boolean_=) float_* boolean_or boolean_and float_- float_- float_sqrt e true float_- boolean_not float_absolute float_positive float_positive true float_negative float_negative exec_if (boolean_= exec_dup (float_= float_* boolean_or boolean_or true boolean_and e float_= exec_dup (exec_if (float_negative boolean_and false) (boolean_or float_absolute true float_negative float_= true boolean_not float_negative pi exec_dup (pi false boolean_= pi pi boolean_and boolean_= e))))) ()))))))))) ()))
;;; Best total error: 445.23597717285156
;;; Best errors: (40.24629211425781 33.53925323486328 19.5482177734375 21.03070068359375 38.94059753417969 5.746498107910156 40.85640335083008 8.713058471679688 47.754451751708984 23.606678009033203 21.420570373535156 8.736053466796875 27.805709838867188 22.17009735107422 14.959064483642578 37.81361389160156 15.032035827636719 16.95769500732422 0.35898590087890625)
;;; Best behaviors: (51.753708 54.460747 61.548218 68.9693 51.059402 59.2535 49.143597 50.71306 42.24555 41.393322 68.57943 50.736053 69.80571 67.8299 50.040936 53.813614 51.967964 50.042305 42.358986)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: (boolean_and exec_dup e in1 float_- false exec_if true float_negative float_- close float_negative float_negative float_* boolean_or exec_dup float_sqrt boolean_or float_negative float_absolute float_sqrt close true boolean_= boolean_not false float_- float_negative float_positive boolean_and boolean_or float_positive float_+ boolean_= false float_* boolean_not float_= true float_sqrt float_positive float_* exec_if boolean_= boolean_= exec_if exec_dup in1 float_sqrt float_+ float_positive true float_positive float_- e exec_dup float_sqrt boolean_and float_+ float_absolute float_- float_positive float_positive false boolean_not boolean_and pi exec_dup float_positive boolean_= false float_sqrt float_negative false boolean_and true close float_- float_absolute float_negative boolean_= float_+ close in1 float_sqrt boolean_or exec_dup true float_positive boolean_and true float_absolute true float_* true float_negative boolean_and float_* float_positive boolean_= boolean_or exec_if float_negative float_= boolean_and float_= in1 boolean_not float_negative boolean_or pi close boolean_not float_sqrt in1 float_= exec_dup boolean_and e float_= boolean_not pi boolean_not float_= float_absolute boolean_and float_+ float_positive boolean_not float_+ float_= pi float_- e float_- float_= float_positive boolean_or pi boolean_= float_* float_absolute pi boolean_= exec_dup boolean_or false float_+ boolean_not float_* false float_negative true exec_if exec_if float_= float_= float_- float_+ float_negative float_positive float_sqrt e float_positive in1 boolean_= close exec_if float_= float_negative float_sqrt close in1 pi exec_dup pi false close e float_* float_* true false boolean_or e false float_negative true float_- false float_positive float_sqrt boolean_and float_+ in1 exec_if in1 float_negative boolean_not float_sqrt float_* float_= float_+ false boolean_not exec_dup pi float_negative float_- float_sqrt float_= float_- e exec_if float_absolute float_- exec_dup pi float_absolute float_* in1 boolean_and true float_sqrt boolean_and float_sqrt pi boolean_= boolean_and boolean_not float_sqrt e e float_sqrt pi pi boolean_and in1 float_= float_positive false float_* true e pi true exec_if close float_* true float_sqrt boolean_not float_* float_sqrt close e float_positive float_* float_absolute float_+ float_+ boolean_= float_sqrt)
;;; Best program: (boolean_and exec_dup (e in1 float_- false exec_if (true float_negative float_-) (float_negative float_negative float_* boolean_or exec_dup (float_sqrt boolean_or float_negative float_absolute float_sqrt) true boolean_= boolean_not false float_- float_negative float_positive boolean_and boolean_or float_positive float_+ (boolean_= false float_* boolean_not float_= true float_sqrt float_positive float_* exec_if (boolean_= boolean_= exec_if (exec_dup (in1 float_sqrt float_+ (float_positive true float_positive float_- e exec_dup (float_sqrt boolean_and float_+ (float_absolute float_- float_positive float_positive false boolean_not boolean_and pi exec_dup (float_positive boolean_= false float_sqrt float_negative false boolean_and true) float_- float_absolute float_negative boolean_= float_+ () in1 float_sqrt boolean_or exec_dup (true float_positive boolean_and true float_absolute true float_* true float_negative boolean_and float_* float_positive boolean_= boolean_or exec_if (float_negative float_= boolean_and float_= in1 boolean_not float_negative boolean_or pi) (boolean_not float_sqrt in1 float_= exec_dup (boolean_and e float_= boolean_not pi boolean_not float_= float_absolute boolean_and float_+ (float_positive boolean_not float_+ (float_= pi float_- e float_- float_= float_positive boolean_or pi boolean_= float_* float_absolute pi boolean_= exec_dup (boolean_or false float_+ (boolean_not float_* false float_negative true exec_if (exec_if (float_= float_= float_- float_+ (float_negative float_positive float_sqrt e float_positive in1 boolean_=) exec_if (float_= float_negative float_sqrt) (in1 pi exec_dup (pi false) e float_* float_* true false boolean_or e false float_negative true float_- false float_positive float_sqrt boolean_and float_+ (in1 exec_if (in1 float_negative boolean_not float_sqrt float_* float_= float_+ (false boolean_not exec_dup (pi float_negative float_- float_sqrt float_= float_- e exec_if (float_absolute float_- exec_dup (pi float_absolute float_* in1 boolean_and true float_sqrt boolean_and float_sqrt pi boolean_= boolean_and boolean_not float_sqrt e e float_sqrt pi pi boolean_and in1 float_= float_positive false float_* true e pi true exec_if () (float_* true float_sqrt boolean_not float_* float_sqrt) e float_positive float_* float_absolute float_+ (float_+ (boolean_= float_sqrt)))) ()))) ()))) ()) ())))))))))))) ()) ()))))
;;; Best total error: 445.23597717285156
;;; Best errors: (40.24629211425781 33.53925323486328 19.5482177734375 21.03070068359375 38.94059753417969 5.746498107910156 40.85640335083008 8.713058471679688 47.754451751708984 23.606678009033203 21.420570373535156 8.736053466796875 27.805709838867188 22.17009735107422 14.959064483642578 37.81361389160156 15.032035827636719 16.95769500732422 0.35898590087890625)
;;; Best behaviors: (51.753708 54.460747 61.548218 68.9693 51.059402 59.2535 49.143597 50.71306 42.24555 41.393322 68.57943 50.736053 69.80571 67.8299 50.040936 53.813614 51.967964 50.042305 42.358986)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: (float_= boolean_and boolean_not boolean_or float_negative false true boolean_not float_positive float_- e float_negative float_+ float_sqrt boolean_not float_absolute boolean_= float_absolute close float_+ in1 boolean_and e boolean_not float_positive boolean_not boolean_and boolean_= float_= close true float_= close boolean_not e exec_if float_sqrt exec_if false float_- float_negative in1 boolean_not float_absolute boolean_not float_sqrt boolean_or float_- boolean_and exec_if pi boolean_and float_* float_sqrt float_negative boolean_not boolean_= boolean_or exec_if float_- float_- float_negative float_- e boolean_or boolean_not float_= close float_+ boolean_not false boolean_and float_absolute float_+ boolean_not close boolean_or float_- float_negative float_= exec_if boolean_and boolean_not e float_positive boolean_not e close close true exec_dup float_* boolean_not float_+ float_negative close boolean_and true float_+ boolean_not pi exec_if in1 boolean_not float_+ float_positive in1 float_sqrt float_= exec_dup float_sqrt e float_negative close boolean_not float_- float_positive close false boolean_not float_positive in1 float_= true float_positive float_sqrt exec_dup float_negative close exec_dup close e float_negative float_= true float_sqrt float_* float_+ false float_negative in1 boolean_or true boolean_or false e false e exec_if e float_absolute float_+ pi boolean_or exec_if float_* false boolean_= e float_- exec_dup boolean_=)
;;; Best program: (float_= boolean_and boolean_not boolean_or float_negative false true boolean_not float_positive float_- e float_negative float_+ (float_sqrt boolean_not float_absolute boolean_= float_absolute) float_+ (in1 boolean_and e boolean_not float_positive boolean_not boolean_and boolean_= float_=) true float_= boolean_not e exec_if (float_sqrt exec_if (false float_- float_negative in1 boolean_not float_absolute boolean_not float_sqrt boolean_or float_- boolean_and exec_if (pi boolean_and float_* float_sqrt float_negative boolean_not boolean_= boolean_or exec_if (float_- float_- float_negative float_- e boolean_or boolean_not float_=) (float_+ (boolean_not false boolean_and float_absolute float_+ (boolean_not) boolean_or float_- float_negative float_= exec_if (boolean_and boolean_not e float_positive boolean_not e) () true exec_dup (float_* boolean_not float_+ (float_negative) boolean_and true float_+ (boolean_not pi exec_if (in1 boolean_not float_+ (float_positive in1 float_sqrt float_= exec_dup (float_sqrt e float_negative) boolean_not float_- float_positive) false boolean_not float_positive in1 float_= true float_positive float_sqrt exec_dup (float_negative) exec_dup () e float_negative float_= true float_sqrt float_* float_+ (false float_negative in1 boolean_or true boolean_or false e false e exec_if (e float_absolute float_+ (pi boolean_or exec_if (float_* false boolean_= e float_- exec_dup (boolean_=)) ())) ())) ()))))) ()) ()) ())
;;; Best total error: 694.8019911244714
;;; Best errors: (89.28171817154096 42.67736641317606 1.8888140618801117 49.28856483846903 49.73832006752491 62.28171817154095 50.409939020872116 1.0074872076511383 49.02421820163727 62.28171817154095 50.25196663290262 0.32528015971183777 4.695898436009884 47.4841800481081 62.28171817154095 13.281718171540955 27.924323111772537 28.261420279741287 2.4156217873096466)
;;; Best behaviors: (2.718281828459045 45.32263358682394 40.11118593811989 40.71143516153097 40.26167993247509 2.718281828459045 39.590060979127884 43.00748720765114 40.97578179836273 2.718281828459045 39.74803336709738 42.32528015971184 46.695898436009884 42.5158199518919 2.718281828459045 2.718281828459045 39.07567688822746 38.73857972025871 39.58437821269035)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 7
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or float_+ float_* pi float_negative false close in1 float_= boolean_or false float_negative close float_positive in1 float_negative float_- float_negative float_+ float_absolute boolean_or exec_dup boolean_or float_negative boolean_not float_sqrt in1 exec_if exec_dup float_positive float_- boolean_= false boolean_= exec_if true float_sqrt float_absolute float_+ boolean_or true boolean_or float_* true float_- float_= float_= float_sqrt exec_dup boolean_= close exec_dup float_sqrt e exec_dup boolean_and pi exec_dup pi boolean_= float_- boolean_not exec_if exec_dup close float_absolute boolean_and float_- close boolean_not true e exec_dup float_+ in1 in1 exec_dup float_sqrt float_* false boolean_= float_sqrt float_= float_negative true float_+ float_absolute boolean_not float_negative float_positive close float_sqrt float_= boolean_or float_negative boolean_and float_+ float_- float_positive exec_dup close float_- boolean_not e float_positive boolean_not close boolean_not true float_* float_sqrt float_= float_- boolean_or true pi exec_dup false pi float_positive float_- false float_positive float_- float_+ boolean_= true pi float_positive close float_positive exec_dup boolean_= e true float_- pi boolean_not boolean_and float_- boolean_or float_= exec_if boolean_not close false float_absolute false float_negative float_negative boolean_or float_- true float_+ boolean_not boolean_= e float_positive float_= boolean_or float_positive boolean_not boolean_or true false boolean_not float_negative e e float_= in1 exec_if false boolean_or boolean_not float_positive exec_if boolean_= float_- boolean_and in1 boolean_and float_- e exec_if boolean_= boolean_or exec_if true e float_- boolean_= boolean_not boolean_not in1 close float_- exec_dup boolean_not boolean_not float_- close float_= float_= float_= exec_if false float_* float_negative pi pi float_absolute float_absolute float_* float_positive true pi float_* float_positive float_+ boolean_not close float_- boolean_not exec_dup boolean_or e float_- float_= float_positive pi e exec_dup float_= in1 close pi float_sqrt boolean_not pi float_absolute boolean_= e pi exec_if float_+ float_absolute float_= boolean_and pi float_positive in1 float_sqrt e float_negative true pi float_- boolean_not close boolean_and in1 float_- boolean_not true boolean_= boolean_not close pi pi false close float_absolute float_sqrt float_- pi float_positive pi float_+ close in1 boolean_or true float_positive boolean_and float_= true float_* close boolean_and exec_dup boolean_and exec_dup e true false float_+ e boolean_= true e exec_if float_* in1 float_+ float_absolute float_negative float_- float_negative float_negative boolean_and boolean_or boolean_and float_+ float_positive boolean_and float_= float_= pi float_- exec_dup boolean_not boolean_not float_sqrt boolean_not float_negative boolean_= float_= boolean_and true float_- boolean_not float_sqrt float_negative float_absolute boolean_or float_sqrt boolean_not e pi e false false float_* exec_dup boolean_not false pi float_negative float_* exec_if float_* float_= float_= boolean_not float_+ boolean_not exec_dup e float_absolute float_= in1 boolean_and boolean_or float_absolute float_+ float_= float_negative float_+ exec_if float_+ e in1 true exec_dup float_absolute close boolean_not exec_dup float_- exec_if float_* pi float_- float_sqrt float_+ boolean_and float_negative float_sqrt float_absolute e)
;;; Best program: (boolean_or float_+ (float_* pi float_negative false) in1 float_= boolean_or false float_negative float_positive in1 float_negative float_- float_negative float_+ (float_absolute boolean_or exec_dup (boolean_or float_negative boolean_not float_sqrt in1 exec_if (exec_dup (float_positive float_- boolean_= false boolean_= exec_if (true float_sqrt float_absolute float_+ (boolean_or true boolean_or float_* true float_- float_= float_= float_sqrt exec_dup (boolean_=) exec_dup (float_sqrt e exec_dup (boolean_and pi exec_dup (pi boolean_= float_- boolean_not exec_if (exec_dup () float_absolute boolean_and float_-) (boolean_not true e exec_dup (float_+ (in1 in1 exec_dup (float_sqrt float_* false boolean_= float_sqrt float_= float_negative true float_+ (float_absolute boolean_not float_negative float_positive) float_sqrt float_= boolean_or float_negative boolean_and float_+ (float_- float_positive exec_dup () float_- boolean_not e float_positive boolean_not) boolean_not true float_* float_sqrt float_= float_- boolean_or true pi exec_dup (false pi float_positive float_- false float_positive float_- float_+ (boolean_= true pi float_positive) float_positive exec_dup (boolean_= e true float_- pi boolean_not boolean_and float_- boolean_or float_= exec_if (boolean_not) (false float_absolute false float_negative float_negative boolean_or float_- true float_+ (boolean_not boolean_= e float_positive float_= boolean_or float_positive boolean_not boolean_or true false boolean_not float_negative e e float_= in1 exec_if (false boolean_or boolean_not float_positive exec_if (boolean_= float_- boolean_and in1 boolean_and float_- e exec_if (boolean_= boolean_or exec_if (true e float_- boolean_= boolean_not boolean_not in1) (float_- exec_dup (boolean_not boolean_not float_-) float_= float_= float_= exec_if (false float_* float_negative pi pi float_absolute float_absolute float_* float_positive true pi float_* float_positive float_+ (boolean_not) float_- boolean_not exec_dup (boolean_or e float_- float_= float_positive pi e exec_dup (float_= in1) pi float_sqrt boolean_not pi float_absolute boolean_= e pi exec_if (float_+ (float_absolute float_= boolean_and pi float_positive in1 float_sqrt e float_negative true pi float_- boolean_not) boolean_and in1 float_- boolean_not true boolean_= boolean_not) (pi pi false) float_absolute float_sqrt float_- pi float_positive pi float_+ () in1 boolean_or true float_positive boolean_and float_= true float_*) boolean_and exec_dup (boolean_and exec_dup (e true false float_+ (e boolean_= true e exec_if (float_* in1 float_+ (float_absolute float_negative float_- float_negative float_negative boolean_and boolean_or boolean_and float_+ (float_positive boolean_and float_= float_= pi float_- exec_dup (boolean_not boolean_not float_sqrt boolean_not float_negative boolean_= float_= boolean_and true float_- boolean_not float_sqrt float_negative float_absolute boolean_or float_sqrt boolean_not e pi e false false float_* exec_dup (boolean_not false pi float_negative float_* exec_if (float_* float_= float_= boolean_not float_+ (boolean_not exec_dup (e float_absolute float_= in1 boolean_and boolean_or float_absolute float_+ (float_= float_negative float_+ (exec_if (float_+ (e in1 true exec_dup (float_absolute) boolean_not exec_dup (float_- exec_if (float_* pi float_- float_sqrt float_+ (boolean_and float_negative float_sqrt float_absolute e)) ()))) ()))))) ()))))) ())))) ())) ()) ()) ()))))))))))))) ())) ())))
;;; Best total error: 693.8061929390037
;;; Best errors: (89.28171817154096 42.59370040893555 1.7439002990722656 49.204898834228516 49.58340072631836 62.28171817154095 50.272098541259766 1.1524009704589844 48.859901428222656 62.28171817154095 50.16830062866211 0.4667015075683594 4.7958984375 47.37929916381836 62.28171817154095 13.281718171540955 27.782901763916016 28.119998931884766 2.274200439453125)
;;; Best behaviors: (2.718281828459045 45.4063 40.2561 40.7951 40.4166 2.718281828459045 39.7279 43.1524 41.1401 2.718281828459045 39.8317 42.4667 46.7959 42.6207 2.718281828459045 2.718281828459045 39.2171 38.88 39.7258)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; -------------------------------------------------------
;;; Best plushy: (float_= close float_sqrt e float_sqrt boolean_or in1 boolean_= exec_dup float_negative boolean_= float_+ float_- float_- boolean_or float_negative boolean_not float_- float_absolute boolean_or close exec_dup in1 float_sqrt e float_absolute float_sqrt float_negative in1 boolean_= boolean_or exec_if float_* in1 boolean_= boolean_and close in1 exec_dup boolean_not boolean_not exec_if exec_if exec_if boolean_and float_positive float_+ exec_if boolean_not e boolean_and pi float_* float_* float_= exec_dup pi boolean_not in1 in1 float_positive float_negative float_- float_positive float_absolute float_+ exec_if float_+ true in1 float_* boolean_not float_+ float_negative in1 float_absolute float_sqrt boolean_or in1 boolean_not float_positive boolean_or float_= float_absolute boolean_= false float_- close float_+ float_positive float_negative float_+ true float_absolute float_* pi true float_sqrt float_sqrt in1 true true e boolean_not close exec_if boolean_= exec_if float_+ close float_negative float_positive boolean_or float_absolute float_sqrt pi in1 false boolean_and float_- float_negative e in1 float_absolute float_* float_absolute close float_positive in1 exec_if boolean_not boolean_not false float_absolute exec_dup e float_+ pi float_negative false float_= exec_if float_absolute float_* in1 exec_dup exec_if exec_if pi boolean_or boolean_not boolean_and float_* float_positive close float_absolute float_sqrt boolean_= boolean_and float_absolute float_* boolean_or e false boolean_= float_= float_* float_+ exec_dup float_negative pi pi float_+ boolean_not float_absolute pi float_* boolean_= float_negative float_sqrt in1 close float_* exec_dup boolean_and in1 pi float_sqrt float_positive pi exec_dup e float_negative float_+ in1 close float_- boolean_= float_* e float_+ float_- float_negative exec_if pi true boolean_and close e float_* exec_if boolean_or true true e float_sqrt float_negative pi float_+ float_positive float_absolute float_sqrt float_= in1 float_negative close close false float_absolute in1 boolean_or float_+ exec_if float_absolute float_positive e float_- float_sqrt pi float_- exec_dup boolean_= boolean_= float_sqrt float_positive float_- float_= float_= boolean_= exec_if float_+ boolean_= in1 exec_if boolean_= exec_if float_negative float_sqrt float_- float_+ float_+ pi float_negative boolean_and float_negative boolean_or in1 float_+ exec_dup boolean_or boolean_= float_= float_- float_absolute false close float_= boolean_not exec_dup pi boolean_not float_positive close boolean_not boolean_= float_= float_* pi float_= pi float_absolute true float_- float_positive boolean_and float_sqrt false float_absolute close exec_if float_negative boolean_not close boolean_and float_negative boolean_or boolean_or exec_if float_negative in1 true boolean_not float_absolute false float_= boolean_= exec_dup pi float_sqrt float_= boolean_not false pi float_+ true boolean_= float_- float_+ boolean_and float_+ float_positive float_absolute float_negative pi float_sqrt false float_absolute exec_dup boolean_not pi exec_if e boolean_or exec_if float_- float_absolute close true float_sqrt boolean_and e boolean_or e float_absolute pi float_sqrt pi false float_absolute close in1 float_positive boolean_= boolean_not float_- boolean_= boolean_and exec_if float_positive boolean_and false exec_dup exec_if float_= close exec_dup float_negative float_negative boolean_not exec_dup false float_positive float_- boolean_or exec_if exec_dup boolean_= boolean_and float_absolute float_* exec_if)
;;; Best program: (float_= float_sqrt e float_sqrt boolean_or in1 boolean_= exec_dup (float_negative boolean_= float_+ (float_- float_- boolean_or float_negative boolean_not float_- float_absolute boolean_or) exec_dup (in1 float_sqrt e float_absolute float_sqrt float_negative in1 boolean_= boolean_or exec_if (float_* in1 boolean_= boolean_and) (in1 exec_dup (boolean_not boolean_not exec_if (exec_if (exec_if (boolean_and float_positive float_+ (exec_if (boolean_not e boolean_and pi float_* float_* float_= exec_dup (pi boolean_not in1 in1 float_positive float_negative float_- float_positive float_absolute float_+ (exec_if (float_+ (true in1 float_* boolean_not float_+ (float_negative in1 float_absolute float_sqrt boolean_or in1 boolean_not float_positive boolean_or float_= float_absolute boolean_= false float_-) float_+ (float_positive float_negative float_+ (true float_absolute float_* pi true float_sqrt float_sqrt in1 true true e boolean_not) exec_if (boolean_= exec_if (float_+ () float_negative float_positive boolean_or float_absolute float_sqrt pi in1 false boolean_and float_- float_negative e in1 float_absolute float_* float_absolute) (float_positive in1 exec_if (boolean_not boolean_not false float_absolute exec_dup (e float_+ (pi float_negative false float_= exec_if (float_absolute float_* in1 exec_dup (exec_if (exec_if (pi boolean_or boolean_not boolean_and float_* float_positive) (float_absolute float_sqrt boolean_= boolean_and float_absolute float_* boolean_or e false boolean_= float_= float_* float_+ (exec_dup (float_negative pi pi float_+ (boolean_not float_absolute pi float_* boolean_= float_negative float_sqrt in1) float_* exec_dup (boolean_and in1 pi float_sqrt float_positive pi exec_dup (e float_negative float_+ (in1) float_- boolean_= float_* e float_+ (float_- float_negative exec_if (pi true boolean_and) (e float_* exec_if (boolean_or true true e float_sqrt float_negative pi float_+ (float_positive float_absolute float_sqrt float_= in1 float_negative)) (false float_absolute in1 boolean_or float_+ (exec_if (float_absolute float_positive e float_- float_sqrt pi float_- exec_dup (boolean_= boolean_= float_sqrt float_positive float_- float_= float_= boolean_= exec_if (float_+ (boolean_= in1 exec_if (boolean_= exec_if (float_negative float_sqrt float_- float_+ (float_+ (pi float_negative boolean_and float_negative boolean_or in1 float_+ (exec_dup (boolean_or boolean_= float_= float_- float_absolute false) float_= boolean_not exec_dup (pi boolean_not float_positive) boolean_not boolean_= float_= float_* pi float_= pi float_absolute true float_- float_positive boolean_and float_sqrt false float_absolute) exec_if (float_negative boolean_not) (boolean_and float_negative boolean_or boolean_or exec_if (float_negative in1 true boolean_not float_absolute false float_= boolean_= exec_dup (pi float_sqrt float_= boolean_not false pi float_+ (true boolean_= float_- float_+ (boolean_and float_+ (float_positive float_absolute float_negative pi float_sqrt false float_absolute exec_dup (boolean_not pi exec_if (e boolean_or exec_if (float_- float_absolute) (true float_sqrt boolean_and e boolean_or e float_absolute pi float_sqrt pi false float_absolute) in1 float_positive boolean_= boolean_not float_- boolean_= boolean_and exec_if (float_positive boolean_and false exec_dup (exec_if (float_=) (exec_dup (float_negative float_negative boolean_not exec_dup (false float_positive float_- boolean_or exec_if (exec_dup (boolean_= boolean_and float_absolute float_* exec_if () ())) ()))))) ()) ())))))) ())))) ()) ())) ())) ())))))))))) ())) ()))) ())) ()))) ()))) ())) ()) ()) ())))))
;;; Best total error: 706.0320021063089
;;; Best errors: (92.0 42.338500410318375 1.7282002996653318 48.05259883403778 49.56580072641373 65.0 50.241698540747166 1.162400970235467 48.52390143275261 65.0 50.1579006286338 0.6950015127658844 5.329398453235626 47.09659916162491 65.0 16.0 27.773501764051616 28.100198931992054 2.2663004398345947)
;;; Best behaviors: (0.0 45.661499589681625 40.27179970033467 41.94740116596222 40.43419927358627 0.0 39.758301459252834 43.16240097023547 41.47609856724739 0.0 39.8420993713662 42.695001512765884 47.329398453235626 42.90340083837509 0.0 0.0 39.226498235948384 38.899801068007946 39.733699560165405)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 9
;;; -------------------------------------------------------
;;; Best plushy: (float_negative float_* in1 boolean_not in1 float_sqrt exec_if float_+ float_sqrt close float_- exec_dup float_+ boolean_or boolean_not pi float_+ boolean_= false float_* boolean_not close exec_if in1 float_= float_negative false in1 float_* boolean_not in1 float_positive exec_if close pi boolean_or exec_dup float_- false float_negative boolean_not true exec_if pi boolean_or true boolean_= pi boolean_not boolean_not true float_absolute exec_dup float_negative boolean_= float_sqrt boolean_not float_= boolean_= float_negative false pi float_absolute float_- float_positive true in1 float_sqrt float_negative false float_= exec_if float_* exec_if exec_dup float_negative float_- boolean_and float_= boolean_or exec_dup true float_* e boolean_or float_* float_positive exec_if true in1 float_= float_- exec_if boolean_= close float_positive e float_negative float_= boolean_= float_positive float_negative true false float_- float_positive float_sqrt float_negative float_negative float_negative boolean_or float_* close pi float_- pi boolean_or float_- float_sqrt float_* float_positive float_- float_sqrt float_- e exec_dup exec_dup float_sqrt float_- close pi float_- float_sqrt float_absolute float_positive float_absolute exec_if boolean_not float_negative float_= float_negative exec_if float_= float_= false pi close false boolean_and float_- boolean_or e float_= float_+ float_negative float_+ float_= boolean_or boolean_or float_sqrt float_sqrt in1 in1 float_= boolean_and e boolean_or exec_dup float_positive float_negative float_- boolean_= float_- pi float_positive true boolean_or e e float_- e false in1 float_+ true float_- float_- boolean_not exec_dup float_positive exec_if in1 false boolean_= in1 boolean_not pi boolean_or float_- boolean_not boolean_and boolean_not pi boolean_= float_positive float_+ exec_dup pi boolean_and exec_if float_- close boolean_or pi float_negative boolean_not float_= exec_if in1 e boolean_or float_absolute false boolean_not exec_if float_sqrt false e boolean_and float_absolute boolean_= float_positive pi float_negative exec_if boolean_not in1 pi float_positive float_sqrt boolean_= pi boolean_and pi pi exec_if false exec_dup boolean_or boolean_not exec_if false boolean_= float_- float_= float_absolute in1 boolean_= boolean_= boolean_or boolean_not in1 e float_negative boolean_or float_absolute boolean_or true close boolean_and float_- float_negative float_* float_- boolean_not boolean_and exec_dup in1 e float_= float_- boolean_and float_sqrt float_= true true float_sqrt boolean_not boolean_or float_+ float_- pi float_+ boolean_or exec_if true close float_+ float_absolute boolean_and false float_- float_+ float_- float_positive exec_if in1 float_negative false float_absolute float_sqrt exec_dup float_* float_+ boolean_and e exec_dup float_* boolean_and float_* exec_dup pi exec_if float_+ boolean_and e false float_* float_negative boolean_and float_+ boolean_or float_- float_- boolean_and float_sqrt exec_dup float_- boolean_and boolean_or boolean_or boolean_and float_negative float_negative true boolean_or float_positive false boolean_not exec_dup float_+ float_- e true float_absolute true float_positive boolean_= boolean_not float_= false close float_- boolean_not float_+ exec_if false boolean_and float_* float_- float_sqrt float_sqrt boolean_and close boolean_not pi pi in1 boolean_not float_* false float_sqrt float_* float_positive boolean_or close false float_positive close false boolean_or float_= float_+ false float_positive float_absolute float_sqrt float_absolute close float_positive true e float_positive float_= float_- exec_dup float_sqrt close close close e float_- exec_if float_sqrt float_sqrt close float_sqrt float_positive in1 float_positive e pi true exec_dup float_* float_negative exec_if boolean_or float_negative float_sqrt true float_- in1 false false close close float_+ false float_positive float_* float_positive float_sqrt boolean_= float_= in1 float_positive pi float_+ pi float_= float_= float_sqrt float_= exec_dup boolean_not boolean_not float_sqrt float_+ boolean_and true float_+ float_* exec_if exec_dup in1 e float_negative false exec_dup float_- float_absolute close float_= boolean_not exec_if boolean_or float_* float_absolute close exec_dup float_negative float_absolute exec_if float_positive float_- boolean_or e float_* float_positive float_= true true e boolean_and false boolean_and float_absolute boolean_= float_sqrt float_sqrt boolean_not true boolean_= exec_if float_- boolean_or close boolean_or boolean_not true float_positive boolean_and float_- close float_absolute float_- in1 in1 e boolean_and float_- exec_dup boolean_or close true float_absolute false close float_- float_+ float_- boolean_and boolean_not in1 boolean_= exec_dup float_- close exec_if close in1 float_* e e in1 float_= float_negative boolean_or false float_* float_+ close boolean_or in1 boolean_and float_absolute close boolean_not float_* float_= boolean_and float_absolute true float_- in1 exec_if pi float_- exec_dup boolean_and e in1 float_absolute boolean_and float_+ exec_if float_absolute float_- exec_if float_= false false false float_+ true float_* float_absolute float_positive pi true float_negative in1 boolean_or exec_dup float_* boolean_not float_- float_* boolean_= float_- close boolean_= float_absolute float_= float_- false in1 in1 boolean_and e close e e float_* float_positive exec_if float_negative close false float_sqrt float_* float_* float_negative float_= float_positive float_* float_positive exec_if float_sqrt float_+ in1 float_positive boolean_= boolean_not exec_if exec_dup float_- boolean_= float_* boolean_and true e float_positive float_negative pi false float_sqrt float_* float_positive false boolean_or pi boolean_or in1 float_+ e boolean_= float_+ false boolean_not float_= boolean_= boolean_and float_sqrt exec_dup exec_if pi float_* float_= true false float_positive exec_if pi in1 float_= boolean_not float_sqrt exec_dup pi in1 boolean_= true pi pi boolean_or boolean_not float_- true false float_* true exec_if float_negative e float_negative exec_dup float_sqrt float_+ exec_dup float_positive exec_dup exec_dup true in1 float_+ float_* in1 false boolean_or e in1 close exec_dup float_* boolean_= float_= exec_if pi float_positive float_sqrt exec_dup exec_dup float_= float_negative true float_absolute pi exec_if false close true boolean_not true pi boolean_not float_* boolean_and float_- e boolean_not in1 boolean_= float_negative exec_if e float_* true float_* float_negative close boolean_or in1 true float_= pi float_* float_sqrt float_- true float_- exec_dup pi float_negative float_+ float_sqrt float_+ boolean_or close float_absolute float_* pi float_positive boolean_not float_* exec_if exec_dup float_+ float_= false boolean_= true boolean_or float_- e e true exec_if exec_if boolean_not boolean_and boolean_or float_sqrt close boolean_and in1 exec_if in1 exec_dup float_* pi float_= float_positive boolean_or boolean_and boolean_= exec_dup float_+ float_sqrt false e e float_+ true float_absolute false boolean_or float_absolute in1 true pi float_* in1 boolean_or pi float_* e float_- boolean_not true true close boolean_not false float_* float_absolute float_absolute boolean_= float_+ e boolean_= float_positive float_positive boolean_= pi float_= float_absolute false float_= boolean_not exec_dup boolean_= e boolean_not boolean_or pi float_= false boolean_not true boolean_= float_= boolean_not float_= boolean_not false close pi true in1 close in1 exec_if float_- float_+ false float_= float_- float_negative float_+ boolean_or float_= float_- float_negative float_absolute float_* boolean_not false false float_+ in1 float_absolute exec_if exec_if boolean_= float_+ exec_dup float_sqrt exec_if true float_= boolean_and true boolean_or true boolean_and in1 boolean_or float_* float_+ float_- pi boolean_not float_sqrt float_+ boolean_or e close float_absolute boolean_= e close float_- close float_positive float_- false exec_dup true in1 float_absolute float_positive close false e true in1 in1 e float_sqrt close in1 float_positive exec_dup exec_dup true float_- in1 boolean_and float_- float_* in1 close float_negative float_- float_negative e boolean_or float_- float_absolute pi boolean_or float_absolute close boolean_and false exec_dup exec_dup false float_negative float_positive exec_dup boolean_not boolean_not float_+ e float_positive float_positive float_sqrt in1 boolean_= float_positive exec_dup float_positive boolean_or boolean_or true float_positive float_absolute false boolean_= float_sqrt close boolean_or in1 boolean_and float_absolute boolean_= float_sqrt boolean_not boolean_not boolean_= boolean_and e float_- float_+ float_* float_negative close e false e exec_if float_negative true float_sqrt float_* float_- float_positive true true exec_dup boolean_and boolean_and false float_+ float_negative boolean_not pi float_* float_positive boolean_not close float_negative float_negative float_positive boolean_= boolean_or exec_dup float_- float_- in1 exec_dup float_sqrt float_sqrt float_absolute float_sqrt float_* boolean_not float_= float_* exec_dup pi float_* float_negative float_absolute boolean_or float_= float_- boolean_and in1 boolean_or float_sqrt exec_if float_absolute float_absolute in1 float_= float_sqrt e exec_dup close boolean_or e close true boolean_not boolean_not float_sqrt float_absolute exec_if in1 pi true boolean_not boolean_and boolean_not boolean_= pi boolean_and float_+ float_positive float_+ float_- boolean_not float_= boolean_= close float_negative boolean_or e float_absolute float_sqrt float_* false float_negative boolean_= float_= true in1 float_positive boolean_not exec_if float_sqrt float_= in1 float_* close exec_dup false float_sqrt boolean_or float_sqrt float_= true in1 in1 close exec_dup float_- close exec_dup in1 close close pi true float_negative float_sqrt float_sqrt float_* pi boolean_or float_= close exec_dup e in1 float_- float_positive float_= exec_if boolean_not exec_if float_negative float_* in1 pi close false true boolean_or true pi float_negative close float_- e float_+ float_negative false float_positive float_sqrt boolean_or float_negative float_* close e float_* float_sqrt in1 close boolean_and close float_negative boolean_= boolean_or exec_dup in1 boolean_and close close true float_sqrt float_* exec_dup exec_dup pi boolean_not float_positive float_= boolean_not float_= float_absolute true in1 float_= close close float_* boolean_or float_= e float_+ float_sqrt boolean_and boolean_or exec_dup false in1 float_sqrt false float_sqrt float_sqrt float_sqrt float_- pi float_absolute close pi true exec_dup boolean_not float_absolute float_negative boolean_or e float_+ float_absolute exec_dup exec_if boolean_not in1 boolean_and float_sqrt boolean_and float_negative true float_negative boolean_not boolean_and exec_if float_* boolean_or float_- false pi in1 float_positive float_+ float_= float_= float_+ true float_absolute exec_dup float_= exec_if float_positive float_- false float_negative float_negative float_absolute float_absolute in1 exec_if boolean_and float_- float_= float_negative boolean_not float_- true float_negative float_= boolean_and float_absolute close float_= float_absolute boolean_not float_sqrt float_= float_= e boolean_and float_sqrt in1 close in1 true float_- true boolean_= boolean_= in1 pi float_* boolean_and boolean_or boolean_= close boolean_or in1 float_sqrt float_= exec_if in1 true float_sqrt true boolean_and exec_dup false float_negative boolean_not float_positive boolean_= float_positive false float_absolute boolean_= float_- pi boolean_and float_* boolean_or float_= float_+ float_= close true boolean_= float_* close float_- boolean_or in1 float_negative boolean_or float_+ float_absolute false true float_* e boolean_and float_* false float_absolute float_= true float_= boolean_not in1 float_= pi e float_= exec_if float_positive true close exec_if boolean_or boolean_not float_* boolean_= float_negative float_positive float_sqrt pi float_= boolean_not exec_if close boolean_or float_negative float_- float_- boolean_and false pi close float_- e boolean_or boolean_and exec_dup false boolean_= true in1 boolean_and exec_if float_= float_sqrt boolean_and float_+ boolean_not float_absolute float_- true close boolean_or pi in1 pi true pi float_positive float_- in1 float_absolute false boolean_not in1 boolean_= float_negative e true exec_dup float_- boolean_and float_+ true float_absolute close float_+ pi exec_if float_- exec_dup exec_if boolean_not in1 exec_if float_* boolean_not in1 boolean_or pi float_+ in1 float_negative boolean_or in1 pi close float_+ float_negative boolean_or float_sqrt pi false float_* exec_dup true float_* in1 in1 float_- boolean_or float_negative float_negative boolean_= false boolean_and true true boolean_or boolean_or pi float_positive float_positive true boolean_not pi float_negative exec_dup pi float_* pi in1 float_negative float_+ boolean_= false boolean_not true e boolean_= float_= pi float_- boolean_and boolean_not close false float_positive true boolean_or boolean_not in1 close exec_dup float_* float_+ e float_* exec_if exec_if float_positive boolean_or float_+ true close boolean_not float_* boolean_not float_= boolean_not boolean_= float_+ boolean_and float_- float_sqrt boolean_or float_sqrt float_negative exec_if exec_if pi close boolean_= boolean_and in1 exec_if exec_dup boolean_not boolean_and pi pi false true float_- float_+ float_* float_= boolean_= float_absolute exec_if float_negative float_+ float_=)
;;; Best program: (float_negative float_* in1 boolean_not in1 float_sqrt exec_if (float_+ (float_sqrt) float_- exec_dup (float_+ (boolean_or boolean_not pi float_+ (boolean_= false float_* boolean_not) exec_if (in1 float_= float_negative false in1 float_* boolean_not in1 float_positive exec_if () (pi boolean_or exec_dup (float_- false float_negative boolean_not true exec_if (pi boolean_or true boolean_= pi boolean_not boolean_not true float_absolute exec_dup (float_negative boolean_= float_sqrt boolean_not float_= boolean_= float_negative false pi float_absolute float_- float_positive true in1 float_sqrt float_negative false float_= exec_if (float_* exec_if (exec_dup (float_negative float_- boolean_and float_= boolean_or exec_dup (true float_* e boolean_or float_* float_positive exec_if (true in1 float_= float_- exec_if (boolean_=) (float_positive e float_negative float_= boolean_= float_positive float_negative true false float_- float_positive float_sqrt float_negative float_negative float_negative boolean_or float_*) pi float_- pi boolean_or float_- float_sqrt float_* float_positive float_- float_sqrt float_- e exec_dup (exec_dup (float_sqrt float_-) pi float_- float_sqrt float_absolute float_positive float_absolute exec_if (boolean_not float_negative float_= float_negative exec_if (float_= float_= false pi) (false boolean_and float_- boolean_or e float_= float_+ (float_negative float_+ (float_= boolean_or boolean_or float_sqrt float_sqrt in1 in1 float_= boolean_and e boolean_or exec_dup (float_positive float_negative float_- boolean_= float_- pi float_positive true boolean_or e e float_- e false in1 float_+ (true float_- float_- boolean_not exec_dup (float_positive exec_if (in1 false boolean_= in1 boolean_not pi boolean_or float_- boolean_not boolean_and boolean_not pi boolean_= float_positive float_+ (exec_dup (pi boolean_and exec_if (float_-) (boolean_or pi float_negative boolean_not float_= exec_if (in1 e boolean_or float_absolute false boolean_not exec_if (float_sqrt false e boolean_and float_absolute boolean_= float_positive pi float_negative exec_if (boolean_not in1 pi float_positive float_sqrt boolean_= pi boolean_and pi pi exec_if (false exec_dup (boolean_or boolean_not exec_if (false boolean_= float_- float_= float_absolute in1 boolean_= boolean_= boolean_or boolean_not in1 e float_negative boolean_or float_absolute boolean_or true) (boolean_and float_- float_negative float_* float_- boolean_not boolean_and exec_dup (in1 e float_= float_- boolean_and float_sqrt float_= true true float_sqrt boolean_not boolean_or float_+ (float_- pi float_+ (boolean_or exec_if (true) (float_+ (float_absolute boolean_and false float_- float_+ (float_- float_positive exec_if (in1 float_negative false float_absolute float_sqrt exec_dup (float_* float_+ (boolean_and e exec_dup (float_* boolean_and float_* exec_dup (pi exec_if (float_+ (boolean_and e false float_* float_negative boolean_and float_+ (boolean_or float_- float_- boolean_and float_sqrt exec_dup (float_- boolean_and boolean_or boolean_or boolean_and float_negative float_negative true boolean_or float_positive false boolean_not exec_dup (float_+ (float_- e true float_absolute true float_positive boolean_= boolean_not float_= false) float_- boolean_not float_+ (exec_if (false boolean_and float_* float_- float_sqrt float_sqrt boolean_and) (boolean_not pi pi in1 boolean_not float_* false float_sqrt float_* float_positive boolean_or) false float_positive) false boolean_or float_= float_+ (false float_positive float_absolute float_sqrt float_absolute) float_positive true e float_positive float_= float_- exec_dup (float_sqrt))) e float_- exec_if (float_sqrt float_sqrt) (float_sqrt float_positive in1 float_positive e pi true exec_dup (float_* float_negative exec_if (boolean_or float_negative float_sqrt true float_- in1 false false) () float_+ (false float_positive float_* float_positive float_sqrt boolean_= float_= in1 float_positive pi float_+ (pi float_= float_= float_sqrt float_= exec_dup (boolean_not boolean_not float_sqrt float_+ (boolean_and true float_+ (float_* exec_if (exec_dup (in1 e float_negative false exec_dup (float_- float_absolute) float_= boolean_not exec_if (boolean_or float_* float_absolute) (exec_dup (float_negative float_absolute exec_if (float_positive float_- boolean_or e float_* float_positive float_= true true e boolean_and false boolean_and float_absolute boolean_= float_sqrt float_sqrt boolean_not true boolean_= exec_if (float_- boolean_or) (boolean_or boolean_not true float_positive boolean_and float_-) float_absolute float_- in1 in1 e boolean_and float_- exec_dup (boolean_or) true float_absolute false) (float_- float_+ (float_- boolean_and boolean_not in1 boolean_= exec_dup (float_-) exec_if () (in1 float_* e e in1 float_= float_negative boolean_or false float_* float_+ () boolean_or in1 boolean_and float_absolute) boolean_not float_* float_= boolean_and float_absolute true float_- in1 exec_if (pi float_- exec_dup (boolean_and e in1 float_absolute boolean_and float_+ (exec_if (float_absolute float_- exec_if (float_= false false false float_+ (true float_* float_absolute float_positive pi true float_negative in1 boolean_or exec_dup (float_* boolean_not float_- float_* boolean_= float_-) boolean_= float_absolute float_= float_- false in1 in1 boolean_and e) e e float_* float_positive exec_if (float_negative) (false float_sqrt float_* float_* float_negative float_= float_positive float_* float_positive exec_if (float_sqrt float_+ (in1 float_positive boolean_= boolean_not exec_if (exec_dup (float_- boolean_= float_* boolean_and true e float_positive float_negative pi false float_sqrt float_* float_positive false boolean_or pi boolean_or in1 float_+ (e boolean_= float_+ (false boolean_not float_= boolean_= boolean_and float_sqrt exec_dup (exec_if (pi float_* float_= true false float_positive exec_if (pi in1 float_= boolean_not float_sqrt exec_dup (pi in1 boolean_= true pi pi boolean_or boolean_not float_- true false float_* true exec_if (float_negative e float_negative exec_dup (float_sqrt float_+ (exec_dup (float_positive exec_dup (exec_dup (true in1 float_+ (float_* in1 false boolean_or e in1) exec_dup (float_* boolean_= float_= exec_if (pi float_positive float_sqrt exec_dup (exec_dup (float_= float_negative true float_absolute pi exec_if (false) (true boolean_not true pi boolean_not float_* boolean_and float_- e boolean_not in1 boolean_= float_negative exec_if (e float_* true float_* float_negative) (boolean_or in1 true float_= pi float_* float_sqrt float_- true float_- exec_dup (pi float_negative float_+ (float_sqrt float_+ (boolean_or) float_absolute float_* pi float_positive boolean_not float_* exec_if (exec_dup (float_+ (float_= false boolean_= true boolean_or float_- e e true exec_if (exec_if (boolean_not boolean_and boolean_or float_sqrt) (boolean_and in1 exec_if (in1 exec_dup (float_* pi float_= float_positive boolean_or boolean_and boolean_= exec_dup (float_+ (float_sqrt false e e float_+ (true float_absolute false boolean_or float_absolute in1 true pi float_* in1 boolean_or pi float_* e float_- boolean_not true true) boolean_not false float_* float_absolute float_absolute boolean_= float_+ (e boolean_= float_positive float_positive boolean_= pi float_= float_absolute false float_= boolean_not exec_dup (boolean_= e boolean_not boolean_or pi float_= false boolean_not true boolean_= float_= boolean_not float_= boolean_not false) pi true in1) in1 exec_if (float_- float_+ (false float_= float_- float_negative float_+ (boolean_or float_= float_- float_negative float_absolute float_* boolean_not false false float_+ (in1 float_absolute exec_if (exec_if (boolean_= float_+ (exec_dup (float_sqrt exec_if (true float_= boolean_and true boolean_or true boolean_and in1 boolean_or float_* float_+ (float_- pi boolean_not float_sqrt float_+ (boolean_or e) float_absolute boolean_= e) float_-) (float_positive float_- false exec_dup (true in1 float_absolute float_positive) false e true in1 in1 e float_sqrt) in1 float_positive exec_dup (exec_dup (true float_- in1 boolean_and float_- float_* in1) float_negative float_- float_negative e boolean_or float_- float_absolute pi boolean_or float_absolute) boolean_and false exec_dup (exec_dup (false float_negative float_positive exec_dup (boolean_not boolean_not float_+ (e float_positive float_positive float_sqrt in1 boolean_= float_positive exec_dup (float_positive boolean_or boolean_or true float_positive float_absolute false boolean_= float_sqrt) boolean_or in1 boolean_and float_absolute boolean_= float_sqrt boolean_not boolean_not boolean_= boolean_and e float_- float_+ (float_* float_negative) e false e exec_if (float_negative true float_sqrt float_* float_- float_positive true true exec_dup (boolean_and boolean_and false float_+ (float_negative boolean_not pi float_* float_positive boolean_not) float_negative float_negative float_positive boolean_= boolean_or exec_dup (float_- float_- in1 exec_dup (float_sqrt float_sqrt float_absolute float_sqrt float_* boolean_not float_= float_* exec_dup (pi float_* float_negative float_absolute boolean_or float_= float_- boolean_and in1 boolean_or float_sqrt exec_if (float_absolute float_absolute in1 float_= float_sqrt e exec_dup () boolean_or e) (true boolean_not boolean_not float_sqrt float_absolute exec_if (in1 pi true boolean_not boolean_and boolean_not boolean_= pi boolean_and float_+ (float_positive float_+ (float_- boolean_not float_= boolean_=) float_negative boolean_or e float_absolute float_sqrt float_* false float_negative boolean_= float_= true in1 float_positive boolean_not exec_if (float_sqrt float_= in1 float_*) (exec_dup (false float_sqrt boolean_or float_sqrt float_= true in1 in1) exec_dup (float_-) exec_dup (in1)) pi true float_negative float_sqrt float_sqrt float_* pi boolean_or float_=) exec_dup (e in1 float_- float_positive float_= exec_if (boolean_not exec_if (float_negative float_* in1 pi) (false true boolean_or true pi float_negative) float_- e float_+ (float_negative false float_positive float_sqrt boolean_or float_negative float_*) e float_* float_sqrt in1) (boolean_and) float_negative boolean_= boolean_or exec_dup (in1 boolean_and)) true float_sqrt float_* exec_dup (exec_dup (pi boolean_not float_positive float_= boolean_not float_= float_absolute true in1 float_=)) float_* boolean_or float_= e float_+ (float_sqrt boolean_and boolean_or exec_dup (false in1 float_sqrt false float_sqrt float_sqrt float_sqrt float_- pi float_absolute) pi true exec_dup (boolean_not float_absolute float_negative boolean_or e float_+ (float_absolute exec_dup (exec_if (boolean_not in1 boolean_and float_sqrt boolean_and float_negative true float_negative boolean_not boolean_and exec_if (float_* boolean_or float_- false pi in1 float_positive float_+ (float_= float_= float_+ (true float_absolute exec_dup (float_= exec_if (float_positive float_- false float_negative float_negative float_absolute float_absolute in1 exec_if (boolean_and float_- float_= float_negative boolean_not float_- true float_negative float_= boolean_and float_absolute) (float_= float_absolute boolean_not float_sqrt float_= float_= e boolean_and float_sqrt in1) in1 true float_- true boolean_= boolean_= in1 pi float_* boolean_and boolean_or boolean_=) (boolean_or in1 float_sqrt float_= exec_if (in1 true float_sqrt true boolean_and exec_dup (false float_negative boolean_not float_positive boolean_= float_positive false float_absolute boolean_= float_- pi boolean_and float_* boolean_or float_= float_+ (float_=) true boolean_= float_*) float_- boolean_or in1 float_negative boolean_or float_+ (float_absolute false true float_* e boolean_and float_* false float_absolute float_= true float_= boolean_not in1 float_= pi e float_= exec_if (float_positive true) (exec_if (boolean_or boolean_not float_* boolean_= float_negative float_positive float_sqrt pi float_= boolean_not exec_if () (boolean_or float_negative float_- float_- boolean_and false pi) float_- e boolean_or boolean_and exec_dup (false boolean_= true in1 boolean_and exec_if (float_= float_sqrt boolean_and float_+ (boolean_not float_absolute float_- true) boolean_or pi in1 pi true pi float_positive float_- in1 float_absolute false boolean_not in1 boolean_= float_negative e true exec_dup (float_- boolean_and float_+ (true float_absolute) float_+ (pi exec_if (float_- exec_dup (exec_if (boolean_not in1 exec_if (float_* boolean_not in1 boolean_or pi float_+ (in1 float_negative boolean_or in1 pi) float_+ (float_negative boolean_or float_sqrt pi false float_* exec_dup (true float_* in1 in1 float_- boolean_or float_negative float_negative boolean_= false boolean_and true true boolean_or boolean_or pi float_positive float_positive true boolean_not pi float_negative exec_dup (pi float_* pi in1 float_negative float_+ (boolean_= false boolean_not true e boolean_= float_= pi float_- boolean_and boolean_not) false float_positive true boolean_or boolean_not in1) exec_dup (float_* float_+ (e float_* exec_if (exec_if (float_positive boolean_or float_+ (true) boolean_not float_* boolean_not float_= boolean_not boolean_= float_+ (boolean_and float_- float_sqrt boolean_or float_sqrt float_negative exec_if (exec_if (pi) (boolean_= boolean_and in1 exec_if (exec_dup (boolean_not boolean_and pi pi false true float_- float_+ (float_* float_= boolean_= float_absolute exec_if (float_negative float_+ (float_=)) ()))) ())) ())) ()) ()))))) ()) ())) ()))) ())) ()))) ()))))) ()) ()))))) ())))))) ()))))))) ()) ())))) ())))) ())) ()))) ()))))))) ()))))))) ())) ()) ()))))) ())) ())) ()) ()))) ())))))) ())))))))))) ()))))) ()))))))))) ()) ()) ()) ())))) ()))))))) ())) ()))) ()) ())) ()))) ()))) ())
;;; Best total error: 706.0320021063089
;;; Best errors: (92.0 42.338500410318375 1.7282002996653318 48.05259883403778 49.56580072641373 65.0 50.241698540747166 1.162400970235467 48.52390143275261 65.0 50.1579006286338 0.6950015127658844 5.329398453235626 47.09659916162491 65.0 16.0 27.773501764051616 28.100198931992054 2.2663004398345947)
;;; Best behaviors: (0.0 45.661499589681625 40.27179970033467 41.94740116596222 40.43419927358627 0.0 39.758301459252834 43.16240097023547 41.47609856724739 0.0 39.8420993713662 42.695001512765884 47.329398453235626 42.90340083837509 0.0 0.0 39.226498235948384 38.899801068007946 39.733699560165405)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 10
;;; -------------------------------------------------------
;;; Best plushy: (false float_= float_= boolean_or pi true e float_= pi false boolean_= exec_dup float_= boolean_= float_+ float_positive in1 exec_dup pi float_* float_* float_= pi true close float_sqrt boolean_= float_sqrt close float_positive float_+ boolean_= in1 boolean_not float_sqrt float_sqrt e true float_- boolean_not boolean_= boolean_not exec_if pi pi boolean_= boolean_or true in1 true boolean_= float_absolute boolean_not float_- boolean_not float_absolute exec_dup float_negative float_positive exec_if float_sqrt pi e boolean_= float_absolute float_+ float_absolute boolean_not boolean_or boolean_not boolean_= exec_if float_+ close boolean_not float_= float_negative float_* boolean_= exec_dup float_= float_= float_* float_absolute float_negative pi boolean_not boolean_= boolean_= boolean_not float_- close float_positive boolean_and false float_- float_+ boolean_or true e exec_dup exec_if float_absolute in1 float_= close float_absolute boolean_= in1 float_* e exec_if float_= float_negative exec_if e true float_+ float_positive boolean_or boolean_not close float_negative pi float_- float_* boolean_= float_* close in1 float_sqrt pi pi float_negative boolean_= boolean_= in1 true boolean_not float_absolute float_negative e float_+ float_- close exec_if close exec_if boolean_or boolean_and boolean_or false true exec_dup false close e e exec_dup float_negative e pi in1 float_= e float_- close true true float_* e e e float_absolute float_= exec_dup float_positive boolean_= float_absolute float_positive float_= boolean_= boolean_or float_+ boolean_or boolean_and e boolean_or close exec_dup in1 e float_= close false float_- float_+ in1 boolean_= float_- boolean_= boolean_and boolean_or float_negative pi float_negative float_negative in1 e float_* float_positive boolean_not float_+ boolean_or true pi in1 pi boolean_not boolean_not boolean_and close exec_if float_negative true boolean_and float_sqrt float_= float_= pi in1 float_- float_positive true false exec_if float_negative float_positive e e exec_if boolean_and float_positive float_negative exec_if true float_+ boolean_= boolean_or e float_negative float_- true exec_if float_- float_absolute float_absolute float_positive in1 e boolean_not boolean_or false float_+ true exec_if e float_absolute boolean_and float_= float_+ float_* e float_= boolean_not false pi close exec_dup float_+ close close exec_if false float_positive false exec_if float_= in1 true float_* true pi true boolean_or true exec_dup true in1 float_* float_absolute exec_dup boolean_or boolean_and float_- boolean_and boolean_or false float_negative boolean_not pi in1 true float_negative float_+ boolean_= pi exec_if float_positive float_* boolean_or float_* float_= exec_if float_* boolean_and float_* float_positive close float_= e pi float_absolute true float_positive close float_* exec_if close float_+ e float_= boolean_= exec_if float_sqrt exec_if float_sqrt float_= true true boolean_not close float_negative float_positive boolean_= e float_sqrt float_= exec_if boolean_= close float_sqrt float_= float_negative float_= pi float_positive float_negative float_- e float_+ float_negative float_* boolean_or float_* false exec_dup boolean_= false true in1 close float_sqrt in1 float_- float_absolute pi float_* float_positive true boolean_not float_+ float_positive float_absolute exec_dup e float_positive float_sqrt boolean_not float_* float_positive float_absolute e exec_dup boolean_and boolean_and float_negative in1 boolean_and boolean_or float_positive exec_dup float_absolute float_positive float_- float_- in1 boolean_and float_absolute boolean_and float_positive float_negative exec_dup float_+ float_+ float_- float_positive close true boolean_= boolean_and false float_sqrt true boolean_= close float_* exec_dup float_negative float_- boolean_and float_negative float_negative false pi float_positive close boolean_or boolean_or boolean_= exec_dup float_* exec_if boolean_not in1 float_positive boolean_and exec_if float_sqrt boolean_or float_sqrt boolean_and float_- float_* boolean_= float_= close boolean_not float_positive boolean_and exec_if in1 float_negative boolean_= boolean_or float_= float_* float_absolute float_sqrt float_positive boolean_= float_absolute close float_absolute boolean_not boolean_= exec_if float_sqrt float_- float_negative float_sqrt pi float_+ float_positive float_+ float_negative pi float_negative float_- float_negative float_= boolean_and float_sqrt boolean_= false float_+ float_absolute true boolean_not pi float_* float_positive float_negative float_- float_+ e float_negative float_* float_= exec_if exec_dup float_negative float_negative e exec_dup boolean_and pi exec_dup float_* float_* false false boolean_= false boolean_not in1 float_sqrt boolean_= close float_sqrt boolean_= boolean_not float_* float_sqrt boolean_or true close false float_* true boolean_not false close boolean_and boolean_= boolean_= float_negative float_* boolean_= false e float_= float_= float_sqrt float_* float_= false exec_dup float_absolute float_positive float_absolute boolean_and float_* boolean_= e exec_dup e float_positive float_= boolean_not boolean_or float_sqrt float_* boolean_= float_sqrt float_sqrt e float_absolute float_negative boolean_and boolean_or exec_if float_+ float_= close true boolean_or exec_dup boolean_or exec_if boolean_or float_+ float_- float_* boolean_and float_absolute float_absolute float_positive float_+ close boolean_not boolean_not boolean_and boolean_and boolean_or true false pi float_negative float_positive float_* float_positive exec_dup boolean_or float_sqrt float_- boolean_or boolean_and exec_dup float_- boolean_and true boolean_= e float_* close float_negative e float_* float_positive exec_dup true false exec_if float_negative float_absolute float_absolute close exec_if boolean_= float_sqrt float_* float_- boolean_= float_sqrt float_absolute float_+ float_sqrt float_* close float_negative e boolean_or boolean_= boolean_not boolean_or in1 exec_dup float_absolute exec_dup float_negative boolean_and pi boolean_= false float_- float_negative float_+ float_positive float_sqrt false boolean_not true float_sqrt float_negative exec_dup float_= float_sqrt close in1 float_positive boolean_and exec_if pi true e false float_= boolean_or boolean_= float_sqrt boolean_and boolean_and float_absolute e close boolean_= float_* boolean_not boolean_and boolean_not boolean_and in1 e boolean_and boolean_and boolean_= exec_dup float_* float_= in1 float_sqrt false float_sqrt true float_positive boolean_not boolean_= float_* float_= float_= float_sqrt float_* boolean_= false float_= exec_dup float_negative float_positive float_= boolean_or float_sqrt float_+ false in1 false false in1 exec_dup exec_dup float_negative true float_- float_- boolean_= false exec_if boolean_not float_= exec_dup float_= boolean_= float_positive pi close pi float_negative close float_absolute pi in1 pi exec_if true pi float_- float_positive false float_= float_+ pi exec_if in1 exec_dup close in1 float_* float_positive float_sqrt float_+ float_absolute float_sqrt float_sqrt pi boolean_and boolean_or true pi false boolean_or exec_if float_positive float_= exec_if close float_+ boolean_= false in1 boolean_and float_- pi exec_if true boolean_not true e float_- float_positive float_+ true float_positive float_= boolean_and float_sqrt boolean_or float_= float_* boolean_= boolean_= boolean_or float_negative float_negative float_+ true boolean_not float_- float_* close float_+ float_- e float_* float_+ float_negative in1 float_sqrt float_= boolean_not false boolean_or exec_if boolean_= false float_sqrt float_* float_sqrt float_= close float_= true close float_- exec_dup close exec_dup exec_if false close e float_positive boolean_= float_negative pi boolean_or boolean_or boolean_= pi float_sqrt close boolean_= exec_dup false boolean_or boolean_or true float_absolute exec_if float_absolute float_+ float_- true float_positive boolean_and in1 boolean_or float_* float_negative true boolean_and float_* float_+ boolean_not float_* exec_dup in1 float_sqrt float_positive float_absolute float_= in1 float_- boolean_and boolean_not float_* boolean_and pi float_* float_sqrt exec_if false exec_if boolean_not false boolean_or in1 pi float_absolute exec_if boolean_= in1 float_* float_positive float_+ float_positive in1 float_absolute boolean_= exec_dup boolean_or float_- float_= float_+ e float_absolute close boolean_and exec_dup boolean_or boolean_or exec_dup boolean_= boolean_= boolean_or float_positive e close boolean_and float_positive boolean_= false e pi float_* float_* boolean_or close float_- in1 float_negative float_positive float_absolute float_* pi true boolean_or float_+ float_* float_negative in1 pi float_negative float_sqrt exec_if boolean_= float_positive boolean_and float_absolute true in1 float_sqrt float_sqrt in1 float_absolute boolean_and boolean_or exec_dup exec_if float_+ float_- e boolean_= e in1 exec_dup true float_* exec_if float_* float_- exec_dup float_+ float_positive e false exec_dup exec_if boolean_not float_absolute float_* exec_if boolean_not float_= float_+ float_+ float_positive boolean_and close boolean_not close float_positive float_negative boolean_and boolean_not exec_if float_= boolean_not exec_if true float_positive boolean_and false float_+ boolean_not boolean_or boolean_or float_absolute float_sqrt in1 boolean_= float_negative exec_if close close boolean_= close boolean_not boolean_or float_absolute float_* float_sqrt exec_dup exec_if pi float_negative float_- exec_if true boolean_or float_- boolean_not true exec_dup exec_if pi boolean_not float_negative exec_dup e exec_dup float_+ boolean_and float_negative true float_= float_sqrt float_sqrt float_sqrt boolean_or float_- float_absolute boolean_= float_sqrt e float_positive boolean_or float_positive true exec_dup boolean_not float_absolute in1 in1 e float_= in1 false boolean_and boolean_or e float_positive float_absolute boolean_not boolean_= float_absolute exec_dup float_* float_negative float_= true boolean_or float_positive pi e boolean_and close boolean_= true true float_negative false exec_if float_- float_positive exec_dup float_- boolean_and float_positive float_* float_* float_- exec_dup float_* e in1 float_sqrt float_sqrt e exec_if boolean_not boolean_= boolean_or exec_if boolean_and boolean_not float_- pi close boolean_or close boolean_and float_+ boolean_not float_negative float_- boolean_= false e pi float_= e)
;;; Best program: (false float_= float_= boolean_or pi true e float_= pi false boolean_= exec_dup (float_= boolean_= float_+ (float_positive in1 exec_dup (pi float_* float_* float_= pi true) float_sqrt boolean_= float_sqrt) float_positive float_+ (boolean_= in1 boolean_not float_sqrt float_sqrt e true float_- boolean_not boolean_= boolean_not exec_if (pi pi boolean_= boolean_or true in1 true boolean_= float_absolute boolean_not float_- boolean_not float_absolute exec_dup (float_negative float_positive exec_if (float_sqrt pi e boolean_= float_absolute float_+ (float_absolute boolean_not boolean_or boolean_not boolean_= exec_if (float_+ () boolean_not float_= float_negative float_* boolean_= exec_dup (float_= float_= float_* float_absolute float_negative pi boolean_not boolean_= boolean_= boolean_not float_-) float_positive boolean_and false float_- float_+ (boolean_or true e exec_dup (exec_if (float_absolute in1 float_=) (float_absolute boolean_= in1 float_* e exec_if (float_= float_negative exec_if (e true float_+ (float_positive boolean_or boolean_not) float_negative pi float_- float_* boolean_= float_*) (in1 float_sqrt pi pi float_negative boolean_= boolean_= in1 true boolean_not float_absolute float_negative e float_+ (float_-) exec_if () (exec_if (boolean_or boolean_and boolean_or false true exec_dup (false) e e exec_dup (float_negative e pi in1 float_= e float_-) true true float_* e e e float_absolute float_= exec_dup (float_positive boolean_= float_absolute float_positive float_= boolean_= boolean_or float_+ (boolean_or boolean_and e boolean_or) exec_dup (in1 e float_=) false float_- float_+ (in1 boolean_= float_- boolean_= boolean_and boolean_or float_negative pi float_negative float_negative in1 e float_* float_positive boolean_not float_+ (boolean_or true pi in1 pi boolean_not boolean_not boolean_and) exec_if (float_negative true boolean_and float_sqrt float_= float_= pi in1 float_- float_positive true false exec_if (float_negative float_positive e e exec_if (boolean_and float_positive float_negative exec_if (true float_+ (boolean_= boolean_or e float_negative float_- true exec_if (float_- float_absolute float_absolute float_positive in1 e boolean_not boolean_or false float_+ (true exec_if (e float_absolute boolean_and float_= float_+ (float_* e float_= boolean_not false pi) exec_dup (float_+ ()) exec_if (false float_positive false exec_if (float_= in1 true float_* true pi true boolean_or true exec_dup (true in1 float_* float_absolute exec_dup (boolean_or boolean_and float_- boolean_and boolean_or false float_negative boolean_not pi in1 true float_negative float_+ (boolean_= pi exec_if (float_positive float_* boolean_or float_* float_= exec_if (float_* boolean_and float_* float_positive) (float_= e pi float_absolute true float_positive) float_* exec_if () (float_+ (e float_= boolean_= exec_if (float_sqrt exec_if (float_sqrt float_= true true boolean_not) (float_negative float_positive boolean_= e float_sqrt float_= exec_if (boolean_=) (float_sqrt float_= float_negative float_= pi float_positive float_negative float_- e float_+ (float_negative float_* boolean_or float_* false exec_dup (boolean_= false true in1) float_sqrt in1 float_- float_absolute pi float_* float_positive true boolean_not float_+ (float_positive float_absolute exec_dup (e float_positive float_sqrt boolean_not float_* float_positive float_absolute e exec_dup (boolean_and boolean_and float_negative in1 boolean_and boolean_or float_positive exec_dup (float_absolute float_positive float_- float_- in1 boolean_and float_absolute boolean_and float_positive float_negative exec_dup (float_+ (float_+ (float_- float_positive) true boolean_= boolean_and false float_sqrt true boolean_=) float_* exec_dup (float_negative float_- boolean_and float_negative float_negative false pi float_positive) boolean_or boolean_or boolean_= exec_dup (float_* exec_if (boolean_not in1 float_positive boolean_and exec_if (float_sqrt boolean_or float_sqrt boolean_and float_- float_* boolean_= float_=) (boolean_not float_positive boolean_and exec_if (in1 float_negative boolean_= boolean_or float_= float_* float_absolute float_sqrt float_positive boolean_= float_absolute) (float_absolute boolean_not boolean_= exec_if (float_sqrt float_- float_negative float_sqrt pi float_+ (float_positive float_+ (float_negative pi float_negative float_- float_negative float_= boolean_and float_sqrt boolean_= false float_+ (float_absolute true boolean_not pi float_* float_positive float_negative float_- float_+ (e float_negative float_* float_= exec_if (exec_dup (float_negative float_negative e exec_dup (boolean_and pi exec_dup (float_* float_* false false boolean_= false boolean_not in1 float_sqrt boolean_=) float_sqrt boolean_= boolean_not float_* float_sqrt boolean_or true) false float_* true boolean_not false) boolean_and boolean_= boolean_= float_negative float_* boolean_= false e float_= float_= float_sqrt float_* float_= false exec_dup (float_absolute float_positive float_absolute boolean_and float_* boolean_= e exec_dup (e float_positive float_= boolean_not boolean_or float_sqrt float_* boolean_= float_sqrt float_sqrt e float_absolute float_negative boolean_and boolean_or exec_if (float_+ (float_=) true boolean_or exec_dup (boolean_or exec_if (boolean_or float_+ (float_- float_* boolean_and float_absolute float_absolute float_positive float_+ () boolean_not boolean_not boolean_and boolean_and boolean_or true false pi float_negative float_positive float_* float_positive exec_dup (boolean_or float_sqrt float_- boolean_or boolean_and exec_dup (float_- boolean_and true boolean_= e float_*) float_negative e float_* float_positive exec_dup (true false exec_if (float_negative float_absolute float_absolute) (exec_if (boolean_= float_sqrt float_* float_- boolean_= float_sqrt float_absolute float_+ (float_sqrt float_*) float_negative e boolean_or boolean_= boolean_not boolean_or in1 exec_dup (float_absolute exec_dup (float_negative boolean_and pi boolean_= false float_- float_negative float_+ (float_positive float_sqrt false boolean_not true float_sqrt float_negative exec_dup (float_= float_sqrt) in1 float_positive boolean_and exec_if (pi true e false float_= boolean_or boolean_= float_sqrt boolean_and boolean_and float_absolute e) (boolean_= float_* boolean_not boolean_and boolean_not boolean_and in1 e boolean_and boolean_and boolean_= exec_dup (float_* float_= in1 float_sqrt false float_sqrt true float_positive boolean_not boolean_= float_* float_= float_= float_sqrt float_* boolean_= false float_= exec_dup (float_negative float_positive float_= boolean_or float_sqrt float_+ (false in1 false false in1 exec_dup (exec_dup (float_negative true float_- float_- boolean_= false exec_if (boolean_not float_= exec_dup (float_= boolean_= float_positive pi) pi float_negative) (float_absolute pi in1 pi exec_if (true pi float_- float_positive false float_= float_+ (pi exec_if (in1 exec_dup () in1 float_* float_positive float_sqrt float_+ (float_absolute float_sqrt float_sqrt pi boolean_and boolean_or true pi false boolean_or exec_if (float_positive float_= exec_if () (float_+ (boolean_= false in1 boolean_and float_- pi exec_if (true boolean_not true e float_- float_positive float_+ (true float_positive float_= boolean_and float_sqrt boolean_or float_= float_* boolean_= boolean_= boolean_or float_negative float_negative float_+ (true boolean_not float_- float_*) float_+ (float_- e float_* float_+ (float_negative in1 float_sqrt float_= boolean_not false boolean_or exec_if (boolean_= false float_sqrt float_* float_sqrt float_=) (float_= true) float_- exec_dup () exec_dup (exec_if (false) (e float_positive boolean_= float_negative pi boolean_or boolean_or boolean_= pi float_sqrt) boolean_= exec_dup (false boolean_or boolean_or true float_absolute exec_if (float_absolute float_+ (float_- true float_positive boolean_and in1 boolean_or float_* float_negative true boolean_and float_* float_+ (boolean_not float_* exec_dup (in1 float_sqrt float_positive float_absolute float_= in1 float_- boolean_and boolean_not float_* boolean_and pi float_* float_sqrt exec_if (false exec_if (boolean_not false boolean_or in1 pi float_absolute exec_if (boolean_= in1 float_* float_positive float_+ (float_positive in1 float_absolute boolean_= exec_dup (boolean_or float_- float_= float_+ (e float_absolute) boolean_and exec_dup (boolean_or boolean_or exec_dup (boolean_= boolean_= boolean_or float_positive e) boolean_and float_positive boolean_= false e pi float_* float_* boolean_or) float_- in1 float_negative float_positive float_absolute float_* pi true boolean_or float_+ (float_* float_negative in1 pi float_negative float_sqrt exec_if (boolean_= float_positive boolean_and float_absolute true in1 float_sqrt float_sqrt in1 float_absolute boolean_and boolean_or exec_dup (exec_if (float_+ (float_- e boolean_= e in1 exec_dup (true float_* exec_if (float_* float_- exec_dup (float_+ (float_positive e false exec_dup (exec_if (boolean_not float_absolute float_* exec_if (boolean_not float_= float_+ (float_+ (float_positive boolean_and) boolean_not) float_positive float_negative boolean_and boolean_not exec_if (float_= boolean_not exec_if (true float_positive boolean_and false float_+ (boolean_not boolean_or boolean_or float_absolute float_sqrt in1 boolean_= float_negative exec_if () () boolean_=) boolean_not boolean_or float_absolute float_* float_sqrt exec_dup (exec_if (pi float_negative float_- exec_if (true boolean_or float_- boolean_not true exec_dup (exec_if (pi boolean_not float_negative exec_dup (e exec_dup (float_+ (boolean_and float_negative true float_= float_sqrt float_sqrt float_sqrt boolean_or float_- float_absolute boolean_= float_sqrt e float_positive boolean_or float_positive true exec_dup (boolean_not float_absolute in1 in1 e float_= in1 false boolean_and boolean_or e float_positive float_absolute boolean_not boolean_= float_absolute exec_dup (float_* float_negative float_= true boolean_or float_positive pi e boolean_and) boolean_= true true float_negative false exec_if (float_- float_positive exec_dup (float_- boolean_and float_positive float_* float_* float_- exec_dup (float_* e in1 float_sqrt float_sqrt e exec_if (boolean_not boolean_= boolean_or exec_if (boolean_and boolean_not float_- pi) (boolean_or) boolean_and float_+ (boolean_not float_negative float_- boolean_= false e pi float_= e)) ()))) ()))))) ())) ()) ())) ()) ()) ()) ())))) ()))) ())) ())))) ()) ()) ())))) ())))))) ()))) ())) ())) ()))))))))))) ()))))) ())) ()))) ()))))) ()))) ())))))))))) ()))) ())))) ()) ()) ())) ())) ()) ()) ()) ()))) ()))) ())))) ())) ())) ())))
;;; Best total error: 707.2688498953396
;;; Best errors: (91.94659292198655 42.600700409151614 1.7649002987891436 49.21189883444458 49.607400726526976 64.9371681483326 50.29109854064882 1.1314009707421064 48.88690142892301 64.94345133466967 50.175300628878176 0.4467015080153942 4.785898437723517 47.390299163758755 64.94030974150114 15.97172566733484 27.80290176346898 28.13999893143773 2.29420043900609)
;;; Best behaviors: (0.053407078013455955 45.399299590848386 40.235099701210856 40.78810116555542 40.392599273473024 0.06283185166739451 39.70890145935118 43.131400970742106 41.11309857107699 0.056548665330320595 39.824699371121824 42.446701508015394 46.78589843772352 42.609700836241245 0.05969025849885755 0.028274332665160298 39.19709823653102 38.86000106856227 39.70579956099391)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 11
;;; -------------------------------------------------------
;;; Best plushy: (exec_dup float_= float_= exec_dup float_sqrt float_positive false true true e close close true float_sqrt float_= true float_- boolean_or true exec_if true boolean_and float_absolute boolean_and boolean_or true float_sqrt e e exec_if in1 pi e float_* e boolean_not in1 true false float_+ false true boolean_not pi close float_negative float_absolute exec_if in1 boolean_and float_= float_- float_sqrt exec_dup boolean_not float_sqrt boolean_not float_- true float_negative float_negative true false float_absolute true float_positive exec_if float_negative exec_if float_= boolean_not float_positive boolean_= float_= exec_dup exec_if float_+ float_* boolean_not float_sqrt boolean_or float_positive float_= float_absolute exec_dup pi exec_dup close pi float_absolute exec_dup exec_dup false float_* e float_- float_= float_positive e true float_positive boolean_not float_- float_* exec_dup float_negative exec_dup float_* exec_if boolean_and float_= false boolean_or float_absolute boolean_and float_negative close boolean_and float_+ boolean_or pi float_- float_= close exec_if close false e boolean_not false close true float_- float_- exec_if close exec_dup float_= float_+ true false float_positive close in1 float_sqrt boolean_or float_sqrt float_* float_positive float_- boolean_= float_- boolean_or exec_if true boolean_= boolean_or boolean_or float_= boolean_not float_positive boolean_and float_sqrt float_* float_+ close float_- float_sqrt boolean_and boolean_or boolean_not boolean_or exec_dup boolean_or float_positive float_sqrt float_positive float_- e exec_dup float_sqrt float_+ true exec_if boolean_= close exec_if pi boolean_not float_absolute float_+ exec_dup in1 float_= pi float_positive pi float_* exec_if true float_* exec_dup float_= boolean_= float_- true boolean_or in1 float_* in1 float_- exec_dup exec_if float_+ float_positive close float_sqrt float_negative float_absolute false pi float_+ true float_positive exec_if boolean_= false exec_if pi exec_dup true float_+ pi boolean_and exec_if exec_dup boolean_and boolean_and boolean_not float_negative true exec_dup boolean_or float_negative float_- float_* boolean_not float_positive float_sqrt float_= pi boolean_= float_negative float_- pi close exec_if float_sqrt float_* exec_if float_positive false e boolean_= float_negative float_* float_- boolean_= float_* boolean_not float_* exec_dup boolean_= boolean_or float_+ boolean_and boolean_= boolean_= float_positive e boolean_= boolean_= true boolean_and boolean_= float_absolute float_+ exec_if pi boolean_or float_positive exec_if exec_if float_positive float_sqrt float_absolute boolean_not exec_dup float_* boolean_and float_+ boolean_or float_negative float_negative float_negative float_+ float_positive boolean_= false float_* exec_dup false exec_dup true boolean_or exec_if close in1 float_sqrt float_negative close boolean_not pi exec_dup float_negative exec_dup float_negative float_negative float_+ e e e exec_if boolean_not false float_* e float_sqrt float_negative pi exec_dup float_sqrt boolean_and float_absolute pi true boolean_and float_absolute float_negative float_positive float_- close boolean_= false pi boolean_not float_+ float_- float_absolute float_+ exec_if exec_dup float_negative boolean_and boolean_= float_= exec_dup float_absolute in1 e float_- in1 float_positive boolean_and boolean_= e exec_dup false e exec_if float_absolute pi float_- exec_dup e boolean_= false true false close boolean_= float_absolute float_absolute float_* float_* close boolean_not float_- float_absolute boolean_not close float_negative float_negative float_sqrt float_* float_+ in1 boolean_= float_negative float_absolute float_- exec_if pi exec_if float_sqrt e true float_negative float_absolute float_positive float_= float_+ true e float_= float_sqrt float_* float_- false pi close e boolean_and boolean_and exec_dup exec_dup true float_sqrt exec_if float_- false pi float_* boolean_not boolean_= exec_dup boolean_not close float_negative float_positive boolean_not exec_if float_- in1 float_absolute true boolean_= false false float_sqrt float_positive float_negative true exec_if float_= exec_if false float_= close exec_dup exec_if exec_if boolean_not float_+ pi exec_dup float_sqrt in1 close float_* float_negative float_= exec_dup boolean_not close boolean_or boolean_not pi boolean_and float_- exec_dup pi float_positive in1 close boolean_or in1 float_= float_absolute boolean_and float_- true float_= float_sqrt close boolean_and float_* pi float_positive close boolean_= in1 boolean_or float_+ e pi exec_if exec_if boolean_= in1 float_absolute pi boolean_and float_- false float_= float_* float_= float_sqrt pi float_- in1 boolean_or close false boolean_not boolean_= true exec_if true boolean_= exec_dup float_+ boolean_and boolean_not boolean_= e boolean_or in1 float_sqrt float_- boolean_or in1 float_positive float_+ e pi in1 boolean_or boolean_and boolean_and boolean_= boolean_= exec_dup float_positive float_- float_+ float_- float_positive close float_= boolean_and float_negative boolean_or boolean_= boolean_or in1 exec_dup boolean_and false float_sqrt e close exec_if float_positive exec_dup e float_- boolean_= float_+ boolean_not boolean_= false boolean_or float_absolute boolean_= exec_if in1 float_positive float_= in1 float_* float_positive close close false float_positive float_sqrt close pi boolean_or boolean_and exec_if float_sqrt float_sqrt e boolean_and float_* true boolean_= boolean_not close float_= float_negative boolean_= float_+ e boolean_or pi boolean_and float_- boolean_= close boolean_= pi float_positive e boolean_= boolean_not float_sqrt boolean_not exec_dup float_+ true float_* boolean_or float_negative e exec_dup float_positive false float_positive float_negative e float_* float_+ float_absolute float_negative float_+ float_positive float_* false boolean_and exec_dup e in1 float_= float_sqrt boolean_and pi float_sqrt false in1 float_positive in1 in1 float_sqrt boolean_and float_negative close float_= float_= boolean_not boolean_= float_= boolean_= float_+ float_sqrt float_sqrt boolean_or float_- boolean_or false float_negative e exec_dup exec_if exec_if float_sqrt pi exec_dup float_= float_* e in1 exec_dup boolean_and e float_absolute close boolean_or e exec_dup float_- float_= false float_+ exec_if float_+ float_* boolean_not e float_sqrt e exec_dup exec_dup boolean_and boolean_and close exec_dup e float_- boolean_or e float_* false exec_if float_- float_sqrt float_* boolean_not float_absolute float_- exec_if float_- boolean_not boolean_and float_positive boolean_not float_- float_negative pi float_sqrt e e true pi true close close e false pi true exec_if false e float_sqrt pi exec_dup float_positive e float_positive float_absolute float_sqrt boolean_or float_+ true true in1 boolean_and float_positive boolean_or e float_positive boolean_or exec_if boolean_= in1 boolean_= boolean_not float_- e boolean_or e float_sqrt float_negative true exec_if close close boolean_= exec_dup float_negative pi boolean_= float_+ exec_if float_- float_positive float_- pi true boolean_not boolean_or float_absolute float_+ boolean_and float_+ exec_if float_absolute true float_* float_+ float_positive boolean_= boolean_= float_- float_negative boolean_not exec_dup in1 float_* float_absolute false false boolean_and boolean_= float_* exec_if float_negative pi exec_dup float_absolute float_positive float_* pi float_positive boolean_= float_negative in1 exec_if float_negative exec_if float_negative exec_dup true exec_dup float_sqrt float_+ e float_- boolean_= e float_= false boolean_or float_absolute close float_negative close float_sqrt in1 float_positive boolean_not exec_dup float_= float_+ float_= float_= boolean_not pi boolean_not float_* float_sqrt in1 float_negative pi boolean_or pi exec_dup float_negative float_sqrt float_- float_positive float_= close float_* false float_positive float_sqrt float_absolute float_negative boolean_and true boolean_and close exec_if float_* float_positive close float_+ boolean_and boolean_not boolean_and false pi true float_absolute float_* float_negative boolean_= close float_+ float_* boolean_or boolean_not exec_if float_- true float_* false boolean_and exec_if exec_if float_= float_negative boolean_= exec_if float_absolute in1 exec_if float_- false false float_sqrt boolean_and exec_if true false float_sqrt boolean_or boolean_not float_absolute boolean_or float_sqrt true boolean_not float_+ exec_dup true float_- float_= true float_positive float_* float_sqrt exec_if boolean_= in1 pi float_+ boolean_or boolean_= true e exec_if in1 float_- float_* float_absolute exec_if true float_absolute float_- float_= float_* pi boolean_or float_negative pi boolean_not boolean_not float_absolute float_negative float_= exec_if boolean_= exec_if exec_if float_absolute float_negative boolean_and close float_positive boolean_or false close e float_- boolean_= pi float_- boolean_not close exec_dup true float_negative true exec_dup e float_absolute true float_negative float_absolute float_positive float_positive boolean_or in1 float_absolute boolean_= e float_absolute boolean_= pi boolean_and in1 in1 boolean_or float_negative boolean_and boolean_not float_sqrt float_- float_positive exec_if float_negative boolean_not in1 float_positive boolean_not float_negative false exec_dup boolean_not float_* float_= boolean_= float_* float_= pi in1 float_- exec_if float_negative boolean_and float_positive exec_if pi boolean_= float_positive exec_if false float_* float_absolute pi exec_dup float_= exec_dup false boolean_= true float_positive exec_dup float_sqrt false boolean_not float_* boolean_and float_absolute float_absolute float_* e float_= in1 float_= in1 boolean_not exec_if float_= e float_positive boolean_or false exec_dup close float_* float_absolute in1 float_negative true exec_if float_+ exec_if close e boolean_not boolean_and boolean_= float_* float_sqrt float_absolute float_+ float_+ false float_= exec_if float_negative exec_if exec_dup float_+ boolean_and float_sqrt pi float_negative boolean_not pi pi float_+ true true boolean_not exec_if boolean_and in1 boolean_not exec_dup exec_dup close float_= boolean_= in1 boolean_not float_negative true float_negative float_sqrt in1 true boolean_not float_- false float_negative e float_* e float_positive boolean_not float_negative boolean_or true float_negative float_absolute float_- float_- e exec_dup float_= true boolean_and false float_positive float_= float_positive float_- e pi float_absolute exec_dup in1 float_* e in1 exec_dup exec_dup float_+ exec_if float_- float_+ boolean_or float_- float_sqrt exec_dup float_- float_positive float_- exec_if float_absolute true float_positive float_negative float_* boolean_and boolean_= false boolean_and float_+ exec_if boolean_= in1 close float_* float_absolute false e pi float_= float_+ float_positive float_* true float_+ boolean_and boolean_= float_positive true float_sqrt float_= float_* pi exec_dup float_= false float_= close pi float_- exec_if false boolean_= boolean_not exec_dup float_sqrt in1 close exec_if false float_sqrt float_negative in1 float_absolute float_sqrt float_absolute float_+ float_- exec_if exec_if float_sqrt float_+ false exec_dup float_= float_positive exec_dup false close float_positive float_+ close boolean_and exec_if float_+ float_= boolean_or float_+ false float_absolute float_negative e boolean_= boolean_= boolean_and boolean_= float_= float_+ float_= float_* float_positive in1 false boolean_or float_* close float_absolute exec_if exec_if pi exec_if boolean_and close float_= float_absolute float_sqrt boolean_and false float_negative in1 pi float_positive e e false float_- float_+ float_sqrt e float_* close close float_absolute e boolean_or close exec_dup boolean_not float_= false float_= false boolean_= true float_- false close boolean_= boolean_= float_- close float_negative boolean_or float_absolute float_- float_absolute float_= close float_negative close exec_if float_- boolean_or boolean_not false float_sqrt in1 float_- exec_dup float_absolute close float_- boolean_= boolean_= e pi float_positive pi exec_dup float_sqrt boolean_or boolean_or float_positive false exec_if float_= float_+ close float_* pi false in1 close float_= float_+ close exec_dup float_- float_+ pi boolean_and boolean_and boolean_and float_absolute float_* boolean_not float_negative float_negative exec_dup float_= e in1 true true float_- float_negative float_absolute close exec_dup float_- e float_* e boolean_not boolean_= float_- float_negative pi e exec_dup exec_if float_negative float_negative exec_dup float_negative float_negative boolean_and false exec_dup float_absolute float_- float_absolute true e boolean_and float_negative boolean_not true float_negative boolean_not float_negative float_absolute true pi close float_- float_= exec_if exec_if exec_if pi close boolean_and float_+ float_= boolean_not float_negative float_positive in1 boolean_not in1 close float_- pi exec_if e exec_dup float_sqrt float_positive float_positive exec_dup float_absolute boolean_not exec_if float_+ float_* boolean_= boolean_not float_* close e float_- false float_- float_= float_+ float_negative in1 float_* float_= exec_dup float_= exec_dup boolean_or exec_dup float_= float_negative pi float_absolute true float_sqrt boolean_= boolean_and boolean_and float_absolute true boolean_not float_* pi boolean_or close false boolean_and float_absolute pi exec_dup float_- float_negative float_+ exec_dup float_positive false float_sqrt boolean_not boolean_= boolean_and float_sqrt boolean_or float_sqrt true e float_+ float_- float_* float_absolute float_* float_* boolean_not in1 true boolean_or boolean_and pi boolean_and false boolean_and close float_absolute boolean_or float_absolute float_= float_absolute float_* float_= close pi false boolean_and float_negative true boolean_and in1 float_+ exec_dup boolean_or in1 e boolean_or float_+ float_negative close float_positive boolean_and true boolean_= float_absolute false float_negative float_- e boolean_not exec_if in1 float_* in1 pi float_positive float_absolute float_* boolean_and e float_negative boolean_not float_positive pi float_* float_negative exec_if e boolean_= float_positive boolean_not close close float_* float_absolute exec_if boolean_and boolean_not boolean_and float_+ float_absolute pi float_+ float_sqrt e exec_dup float_absolute boolean_or boolean_or true false boolean_= exec_if float_absolute float_sqrt float_sqrt false float_* close exec_dup boolean_not float_positive close exec_if float_absolute float_absolute in1 in1 boolean_or float_absolute float_positive boolean_and float_= float_= boolean_or float_absolute in1 pi float_- float_sqrt close boolean_= float_+ close e close boolean_= boolean_or exec_dup boolean_not boolean_and boolean_or boolean_or exec_if float_negative false float_positive float_sqrt float_sqrt float_positive boolean_not in1 float_- float_* pi float_absolute boolean_= in1 boolean_or e float_negative exec_if false exec_if pi boolean_= float_* float_absolute float_* float_negative in1 float_= close boolean_= float_sqrt float_- float_= true float_sqrt float_absolute float_= true float_positive float_+ float_* float_absolute float_- exec_if float_negative boolean_and boolean_not float_negative float_absolute float_- boolean_and boolean_not exec_dup in1 exec_dup exec_if float_sqrt boolean_or float_negative pi true false float_- boolean_= float_positive float_* float_- float_negative float_= float_- boolean_not boolean_not exec_if float_- exec_dup float_absolute float_negative true false float_= float_= in1 float_- float_* pi boolean_not boolean_or boolean_not float_* float_= false true float_- boolean_or e float_positive boolean_= boolean_or boolean_not close exec_if boolean_or boolean_and float_= float_+ false exec_if float_negative boolean_= boolean_not boolean_and boolean_not float_absolute boolean_or false float_- float_- float_negative boolean_and false boolean_or boolean_not boolean_and boolean_and boolean_and boolean_or true float_- boolean_or float_negative boolean_or in1 in1 float_sqrt float_sqrt boolean_= float_sqrt true float_negative boolean_and true false boolean_or float_sqrt boolean_and float_sqrt boolean_not true float_+ boolean_= float_= boolean_not float_positive exec_dup float_+ boolean_= float_absolute float_- float_absolute float_+ boolean_not in1 boolean_= boolean_or float_negative float_- exec_if true float_negative boolean_not float_- float_absolute exec_dup boolean_not float_negative close close e pi float_- e float_negative float_= float_- exec_dup boolean_and e in1 in1 false float_negative float_+ float_positive float_absolute float_negative boolean_and exec_dup true boolean_or float_sqrt close float_- float_absolute float_+ e float_- boolean_= float_sqrt boolean_or float_negative pi true boolean_not float_- float_positive float_- float_* float_positive boolean_= close boolean_= float_sqrt pi float_= boolean_= float_+ pi float_+ boolean_= float_= exec_dup float_sqrt float_+ true close false float_absolute float_negative boolean_and exec_if e close float_positive float_sqrt boolean_not float_positive false pi boolean_= true float_- float_- boolean_= false float_= false float_sqrt boolean_or boolean_= e boolean_not float_* pi boolean_not boolean_= close float_- float_absolute exec_if exec_dup false float_positive exec_dup float_- close close float_* boolean_and exec_dup float_= float_positive float_+ boolean_and pi exec_dup float_= true true float_negative exec_if float_+ exec_if exec_if float_negative float_positive close float_negative pi float_= true float_positive pi false close float_+ float_* boolean_not boolean_not close true pi true float_* pi float_= float_sqrt false float_negative e e float_negative float_+ exec_if float_- float_* false boolean_not float_sqrt true float_= e boolean_not float_+ exec_if boolean_and boolean_not boolean_not boolean_not float_negative float_positive float_= true boolean_not close float_sqrt pi e exec_dup e boolean_and float_sqrt float_negative float_positive false float_absolute e false float_sqrt float_negative exec_dup boolean_= pi float_* boolean_and boolean_or in1 pi boolean_not float_sqrt pi boolean_not float_+ float_absolute float_sqrt float_positive in1 close boolean_and float_* float_= false float_* float_+ float_- true float_+ in1 float_positive boolean_and in1 true float_sqrt boolean_and float_negative true float_negative e float_* float_negative float_sqrt float_= in1 e float_- float_positive in1 false float_positive exec_if e float_absolute float_absolute boolean_not boolean_not float_+ true float_positive float_= float_+ exec_if float_positive pi close boolean_or pi float_sqrt true pi float_= exec_dup false float_= float_negative in1 e float_= boolean_= float_* in1 in1 float_+ float_* true close boolean_not boolean_and e e float_positive boolean_= boolean_or float_* boolean_or float_positive close float_= boolean_not float_= e boolean_or float_* true float_negative float_* float_positive close false close boolean_= exec_dup in1 float_+ exec_if float_* pi boolean_and float_negative float_sqrt float_sqrt boolean_and float_+ boolean_or float_+ float_positive false boolean_= close float_positive float_+ float_* boolean_not float_sqrt close boolean_= true float_* false exec_if pi float_- false pi boolean_not boolean_not float_+ float_- float_= float_+ exec_dup boolean_and in1 pi float_positive float_- exec_dup false float_absolute in1 float_negative float_- exec_if close exec_if exec_if boolean_or boolean_not boolean_= exec_dup false float_negative float_= in1 float_* exec_if boolean_or float_positive exec_dup exec_dup false float_- float_sqrt float_= false float_positive float_- boolean_= float_positive pi boolean_and float_negative float_negative pi boolean_and boolean_not exec_if boolean_and float_* true pi exec_if float_sqrt float_* boolean_and float_positive false exec_if float_negative exec_if boolean_= exec_if float_- e float_- false true boolean_and float_* exec_if float_- float_positive true float_= float_= boolean_not float_- e float_positive e boolean_not float_sqrt pi exec_if true exec_dup boolean_= pi false boolean_or boolean_or float_= float_positive float_sqrt float_* float_+ float_sqrt exec_dup close true true close boolean_and float_positive boolean_= float_positive float_- float_positive float_= boolean_or exec_if float_- float_positive false float_absolute float_sqrt float_negative true float_* exec_if float_negative float_negative pi float_= pi float_negative float_absolute float_negative float_* float_* exec_if exec_if false float_absolute float_* pi boolean_or float_negative boolean_not boolean_or boolean_not in1 true float_= pi float_sqrt boolean_and e boolean_and boolean_not exec_dup boolean_= true boolean_or in1 false close in1 e in1 float_= false false boolean_not close exec_if float_* true float_= float_positive boolean_not float_- in1 boolean_or float_= float_sqrt float_absolute float_sqrt float_absolute boolean_or true float_negative pi e exec_dup close float_positive float_* in1 boolean_and float_sqrt float_sqrt float_- float_* e float_sqrt close float_- boolean_= float_+ float_+ close boolean_= float_+ float_absolute exec_dup float_* exec_if float_absolute boolean_not false boolean_or exec_if float_positive boolean_and float_negative in1 boolean_or close boolean_or float_absolute close false float_- pi false boolean_not boolean_and boolean_not float_positive pi boolean_and exec_dup float_= true float_absolute boolean_= false float_negative float_* pi float_* float_= float_- float_* float_positive in1 boolean_or float_* exec_if in1 in1 boolean_and exec_if e float_negative false e float_= boolean_or boolean_and pi float_+ float_absolute boolean_and pi float_+ float_* float_= exec_dup boolean_and false exec_dup float_- float_positive float_- float_- boolean_= float_sqrt float_+ float_sqrt boolean_not pi true float_- exec_if boolean_and float_+ float_+ boolean_= e boolean_and float_+ float_sqrt in1 float_+ in1 float_sqrt true close float_* float_sqrt pi exec_dup boolean_and float_sqrt true float_- in1 boolean_and pi true float_positive e false boolean_= float_positive close in1 float_* float_absolute boolean_or false false in1 boolean_and in1 pi float_positive float_= boolean_not close boolean_and float_positive true float_= float_- float_absolute boolean_or boolean_not float_absolute boolean_or in1 float_- float_* e boolean_= exec_if float_+ true close close float_absolute close close float_* boolean_not in1 boolean_or float_sqrt float_- float_+ close exec_if boolean_not boolean_not exec_if close exec_dup boolean_and pi boolean_= float_- exec_dup true float_negative float_absolute float_* float_* close float_+ float_* exec_dup boolean_= e false float_sqrt float_* pi exec_if float_+ e float_negative exec_dup float_+ boolean_or float_positive true float_* float_absolute float_positive pi boolean_= boolean_not boolean_and boolean_and boolean_not boolean_= float_positive float_negative in1 false exec_dup float_= float_- boolean_= boolean_and boolean_= float_= float_+ float_positive float_sqrt close float_* float_* float_negative float_absolute false boolean_= exec_dup exec_if false boolean_or float_negative exec_dup pi close true float_- float_- float_sqrt float_sqrt boolean_or boolean_not exec_if boolean_and float_* float_* in1 float_- boolean_not float_* float_positive boolean_= float_absolute float_absolute float_sqrt float_- boolean_or boolean_= boolean_or float_= boolean_or e float_* in1 boolean_and false float_= float_+ float_negative boolean_not float_absolute float_negative boolean_and boolean_and float_+ boolean_or e boolean_= true in1 float_negative false false float_absolute exec_if close float_* float_negative float_- exec_if close pi boolean_not float_- e boolean_or boolean_= float_- float_absolute float_positive float_= float_positive boolean_and boolean_= float_* false boolean_= exec_if e float_sqrt float_positive float_negative float_= float_sqrt float_positive boolean_and pi pi float_absolute e float_+ boolean_and boolean_or float_= float_positive float_= boolean_and pi float_negative float_= float_* in1 boolean_and float_absolute float_positive pi float_negative float_* false exec_dup boolean_not true float_negative false e float_= float_* exec_dup true false float_positive exec_dup boolean_and float_negative exec_dup exec_if exec_if boolean_not true float_* false boolean_= pi exec_dup float_positive float_positive boolean_= float_positive boolean_= in1 boolean_not exec_if exec_if float_+ boolean_or float_positive exec_dup float_* exec_dup boolean_not close close in1 boolean_= close exec_dup e false e boolean_and float_negative float_absolute boolean_and boolean_or in1 boolean_not float_negative exec_dup boolean_= float_+ boolean_= pi in1 boolean_= float_positive boolean_= float_= float_sqrt in1 float_* boolean_or boolean_and false true true close float_* exec_if exec_dup pi exec_dup pi float_* in1 exec_dup float_* e close e exec_dup exec_dup false boolean_not boolean_or in1 pi close float_+ float_negative float_= close close boolean_not exec_if false exec_dup exec_dup float_= float_sqrt true e float_sqrt in1 float_negative pi float_* float_= boolean_= float_* float_- true exec_if boolean_or float_+ boolean_or in1 close float_- boolean_and boolean_and close float_absolute float_+ close boolean_= float_absolute e float_absolute exec_dup float_negative boolean_and exec_if close float_sqrt close float_positive false float_- e float_positive float_- float_absolute float_+ float_- boolean_not float_* boolean_and boolean_not boolean_or float_* float_+ float_* float_- exec_if float_absolute boolean_not float_* float_sqrt boolean_and e float_= true true boolean_or float_absolute exec_if float_absolute float_positive pi boolean_= false e false close pi float_- in1 boolean_= boolean_= float_- float_= float_positive float_* e float_+ false float_positive exec_if float_* float_= exec_dup boolean_and float_absolute float_positive e float_sqrt float_sqrt float_= float_+ boolean_and exec_dup float_- boolean_not float_+ boolean_not float_* boolean_and float_absolute float_= float_sqrt float_absolute false float_+ true float_absolute exec_if float_* float_= boolean_or float_= boolean_not in1 float_+ exec_dup false exec_dup e float_= float_- float_= exec_dup pi float_= float_negative float_positive exec_if float_* e close exec_dup float_- boolean_and false float_- float_* pi e float_sqrt e close float_sqrt close boolean_or float_+ boolean_not float_* boolean_or float_* false pi exec_if float_sqrt float_sqrt true float_absolute float_sqrt true exec_dup float_* float_absolute float_sqrt float_- float_- float_sqrt in1 boolean_= exec_if close float_negative boolean_or pi float_* float_= boolean_not float_absolute boolean_not e float_* in1 close float_sqrt float_+ float_- false float_= boolean_not e float_- float_sqrt exec_dup e pi float_absolute float_absolute float_absolute false boolean_and float_- float_negative false boolean_= exec_if float_- boolean_and float_- false boolean_and in1 boolean_not float_+ boolean_or float_absolute float_negative e float_- float_+ exec_dup pi float_sqrt in1 float_* exec_dup false float_absolute in1 boolean_= float_= close true in1 close float_- float_absolute float_positive close float_= float_= pi exec_if pi false float_sqrt boolean_or exec_if close float_absolute float_negative float_- e close float_positive float_absolute boolean_and e float_* float_+ float_absolute float_- float_* float_positive float_absolute in1 exec_dup exec_dup float_positive float_* exec_dup pi exec_dup float_- float_absolute float_* float_- boolean_or float_= exec_dup boolean_or float_- pi float_positive float_* false float_negative float_negative true float_positive float_positive boolean_and e exec_dup e false close)
;;; Best program: (exec_dup (float_= float_= exec_dup (float_sqrt float_positive false true true e)) true float_sqrt float_= true float_- boolean_or true exec_if (true boolean_and float_absolute boolean_and boolean_or true float_sqrt e e exec_if (in1 pi e float_* e boolean_not in1 true false float_+ (false true boolean_not pi) float_negative float_absolute exec_if (in1 boolean_and float_= float_- float_sqrt exec_dup (boolean_not float_sqrt boolean_not float_- true float_negative float_negative true false float_absolute true float_positive exec_if (float_negative exec_if (float_= boolean_not float_positive boolean_= float_= exec_dup (exec_if (float_+ (float_* boolean_not float_sqrt boolean_or float_positive float_= float_absolute exec_dup (pi exec_dup () pi float_absolute exec_dup (exec_dup (false float_* e float_- float_= float_positive e true float_positive boolean_not float_- float_* exec_dup (float_negative exec_dup (float_* exec_if (boolean_and float_= false boolean_or float_absolute boolean_and float_negative) (boolean_and float_+ (boolean_or pi float_- float_=) exec_if () (false e boolean_not false) true float_- float_- exec_if () (exec_dup (float_= float_+ (true false float_positive) in1 float_sqrt boolean_or float_sqrt float_* float_positive float_- boolean_= float_- boolean_or exec_if (true boolean_= boolean_or boolean_or float_= boolean_not float_positive boolean_and float_sqrt float_* float_+ () float_- float_sqrt boolean_and boolean_or boolean_not boolean_or exec_dup (boolean_or float_positive float_sqrt float_positive float_- e exec_dup (float_sqrt float_+ (true exec_if (boolean_=) (exec_if (pi boolean_not float_absolute float_+ (exec_dup (in1 float_= pi float_positive pi float_* exec_if (true float_* exec_dup (float_= boolean_= float_- true boolean_or in1 float_* in1 float_- exec_dup (exec_if (float_+ (float_positive) float_sqrt float_negative float_absolute false pi float_+ (true float_positive exec_if (boolean_= false exec_if (pi exec_dup (true float_+ (pi boolean_and exec_if (exec_dup (boolean_and boolean_and boolean_not float_negative true exec_dup (boolean_or float_negative float_- float_* boolean_not float_positive float_sqrt float_= pi boolean_= float_negative float_- pi) exec_if (float_sqrt float_* exec_if (float_positive false e boolean_= float_negative float_* float_- boolean_= float_* boolean_not float_* exec_dup (boolean_= boolean_or float_+ (boolean_and boolean_= boolean_= float_positive e boolean_= boolean_= true boolean_and boolean_= float_absolute float_+ (exec_if (pi boolean_or float_positive exec_if (exec_if (float_positive float_sqrt float_absolute boolean_not exec_dup (float_* boolean_and float_+ (boolean_or float_negative float_negative float_negative float_+ (float_positive boolean_= false float_* exec_dup (false exec_dup (true boolean_or exec_if () (in1 float_sqrt float_negative) boolean_not pi exec_dup (float_negative exec_dup (float_negative float_negative float_+ (e e e exec_if (boolean_not false float_* e float_sqrt float_negative pi exec_dup (float_sqrt boolean_and float_absolute pi true boolean_and float_absolute float_negative float_positive float_-) boolean_= false pi boolean_not float_+ (float_- float_absolute float_+ (exec_if (exec_dup (float_negative boolean_and boolean_= float_= exec_dup (float_absolute in1 e float_- in1 float_positive boolean_and boolean_= e exec_dup (false e exec_if (float_absolute pi float_- exec_dup (e boolean_= false true false) boolean_= float_absolute float_absolute float_* float_*) (boolean_not float_- float_absolute boolean_not) float_negative float_negative float_sqrt float_* float_+ (in1 boolean_= float_negative float_absolute float_- exec_if (pi exec_if (float_sqrt e true float_negative float_absolute float_positive float_= float_+ (true e float_= float_sqrt float_* float_- false pi) e boolean_and boolean_and exec_dup (exec_dup (true float_sqrt exec_if (float_- false pi float_* boolean_not boolean_= exec_dup (boolean_not) float_negative float_positive boolean_not exec_if (float_- in1 float_absolute true boolean_= false false float_sqrt float_positive float_negative true exec_if (float_= exec_if (false float_=) (exec_dup (exec_if (exec_if (boolean_not float_+ (pi exec_dup (float_sqrt in1) float_* float_negative float_= exec_dup (boolean_not) boolean_or boolean_not pi boolean_and float_- exec_dup (pi float_positive in1) boolean_or in1 float_= float_absolute boolean_and float_- true float_= float_sqrt) boolean_and float_* pi float_positive) (boolean_= in1 boolean_or float_+ (e pi exec_if (exec_if (boolean_= in1 float_absolute pi boolean_and float_- false float_= float_* float_= float_sqrt pi float_- in1 boolean_or) (false boolean_not boolean_= true exec_if (true boolean_= exec_dup (float_+ (boolean_and boolean_not boolean_= e boolean_or in1 float_sqrt float_- boolean_or in1 float_positive float_+ (e pi in1 boolean_or boolean_and boolean_and boolean_= boolean_= exec_dup (float_positive float_- float_+ (float_- float_positive) float_= boolean_and float_negative boolean_or boolean_= boolean_or in1 exec_dup (boolean_and false float_sqrt e) exec_if (float_positive exec_dup (e float_- boolean_= float_+ (boolean_not boolean_= false boolean_or float_absolute boolean_= exec_if (in1 float_positive float_= in1 float_* float_positive) () false float_positive float_sqrt) pi boolean_or boolean_and exec_if (float_sqrt float_sqrt e boolean_and float_* true boolean_= boolean_not) (float_= float_negative boolean_= float_+ (e boolean_or pi boolean_and float_- boolean_=) boolean_= pi float_positive e boolean_= boolean_not float_sqrt boolean_not exec_dup (float_+ (true float_* boolean_or float_negative e exec_dup (float_positive false float_positive float_negative e float_* float_+ (float_absolute float_negative float_+ (float_positive float_* false boolean_and exec_dup (e in1 float_= float_sqrt boolean_and pi float_sqrt false in1 float_positive in1 in1 float_sqrt boolean_and float_negative) float_= float_= boolean_not boolean_= float_= boolean_= float_+ (float_sqrt float_sqrt boolean_or float_- boolean_or false float_negative e exec_dup (exec_if (exec_if (float_sqrt pi exec_dup (float_= float_* e in1 exec_dup (boolean_and e float_absolute) boolean_or e exec_dup (float_- float_= false float_+ (exec_if (float_+ (float_* boolean_not e float_sqrt e exec_dup (exec_dup (boolean_and boolean_and) exec_dup (e float_- boolean_or e float_* false exec_if (float_- float_sqrt float_* boolean_not float_absolute float_- exec_if (float_- boolean_not boolean_and float_positive boolean_not float_- float_negative pi float_sqrt e e true pi true) () e false pi true exec_if (false e float_sqrt pi exec_dup (float_positive e float_positive float_absolute float_sqrt boolean_or float_+ (true true in1 boolean_and float_positive boolean_or e float_positive boolean_or exec_if (boolean_= in1 boolean_= boolean_not float_- e boolean_or e float_sqrt float_negative true exec_if () () boolean_= exec_dup (float_negative pi boolean_= float_+ (exec_if (float_- float_positive float_- pi true boolean_not boolean_or float_absolute float_+ (boolean_and float_+ (exec_if (float_absolute true float_* float_+ (float_positive boolean_= boolean_= float_- float_negative boolean_not exec_dup (in1 float_* float_absolute false false boolean_and boolean_= float_* exec_if (float_negative pi exec_dup (float_absolute float_positive float_* pi float_positive boolean_= float_negative in1 exec_if (float_negative exec_if (float_negative exec_dup (true exec_dup (float_sqrt float_+ (e float_- boolean_= e float_= false boolean_or float_absolute) float_negative) float_sqrt in1 float_positive boolean_not exec_dup (float_= float_+ (float_= float_= boolean_not pi boolean_not float_* float_sqrt in1 float_negative pi boolean_or pi exec_dup (float_negative float_sqrt float_- float_positive float_=) float_* false float_positive float_sqrt float_absolute float_negative boolean_and true boolean_and) exec_if (float_* float_positive) (float_+ (boolean_and boolean_not boolean_and false pi true float_absolute float_* float_negative boolean_=) float_+ (float_* boolean_or boolean_not exec_if (float_- true float_* false boolean_and exec_if (exec_if (float_= float_negative boolean_= exec_if (float_absolute in1 exec_if (float_- false false float_sqrt boolean_and exec_if (true false float_sqrt boolean_or boolean_not float_absolute boolean_or float_sqrt true boolean_not float_+ (exec_dup (true float_- float_= true float_positive float_* float_sqrt exec_if (boolean_= in1 pi float_+ (boolean_or boolean_= true e exec_if (in1 float_- float_* float_absolute exec_if (true float_absolute float_- float_= float_* pi boolean_or float_negative pi boolean_not boolean_not float_absolute float_negative float_= exec_if (boolean_= exec_if (exec_if (float_absolute float_negative boolean_and) (float_positive boolean_or false) e float_- boolean_= pi float_- boolean_not) (exec_dup (true float_negative true exec_dup (e float_absolute true float_negative float_absolute float_positive float_positive boolean_or in1 float_absolute boolean_= e float_absolute boolean_= pi boolean_and in1 in1 boolean_or float_negative boolean_and boolean_not float_sqrt float_- float_positive exec_if (float_negative boolean_not in1 float_positive boolean_not float_negative false exec_dup (boolean_not float_* float_= boolean_= float_* float_= pi in1 float_- exec_if (float_negative boolean_and float_positive exec_if (pi boolean_= float_positive exec_if (false float_* float_absolute pi exec_dup (float_= exec_dup (false boolean_= true float_positive exec_dup (float_sqrt false boolean_not float_* boolean_and float_absolute float_absolute float_* e float_= in1 float_= in1 boolean_not exec_if (float_= e float_positive boolean_or false exec_dup () float_* float_absolute in1 float_negative true exec_if (float_+ (exec_if () (e boolean_not boolean_and boolean_= float_* float_sqrt float_absolute float_+ (float_+ (false float_= exec_if (float_negative exec_if (exec_dup (float_+ (boolean_and float_sqrt pi float_negative boolean_not pi pi float_+ (true true boolean_not exec_if (boolean_and in1 boolean_not exec_dup (exec_dup () float_= boolean_= in1 boolean_not float_negative true float_negative float_sqrt in1 true boolean_not float_- false float_negative e float_* e float_positive boolean_not float_negative boolean_or true float_negative float_absolute float_- float_- e exec_dup (float_= true boolean_and false float_positive float_= float_positive float_- e pi float_absolute exec_dup (in1 float_* e in1 exec_dup (exec_dup (float_+ (exec_if (float_- float_+ (boolean_or float_- float_sqrt exec_dup (float_- float_positive float_- exec_if (float_absolute true float_positive float_negative float_* boolean_and boolean_= false boolean_and float_+ (exec_if (boolean_= in1) (float_* float_absolute false e pi float_= float_+ (float_positive float_* true float_+ (boolean_and boolean_= float_positive true float_sqrt float_= float_* pi exec_dup (float_= false float_=) pi float_- exec_if (false boolean_= boolean_not exec_dup (float_sqrt in1) exec_if (false float_sqrt float_negative in1 float_absolute float_sqrt float_absolute float_+ (float_- exec_if (exec_if (float_sqrt float_+ (false exec_dup (float_= float_positive exec_dup (false) float_positive float_+ () boolean_and exec_if (float_+ (float_= boolean_or float_+ (false float_absolute float_negative e boolean_= boolean_= boolean_and boolean_= float_= float_+ (float_= float_* float_positive in1 false boolean_or float_*) float_absolute exec_if (exec_if (pi exec_if (boolean_and) (float_= float_absolute float_sqrt boolean_and false float_negative in1 pi float_positive e e false float_- float_+ (float_sqrt e float_*)) float_absolute e boolean_or) (exec_dup (boolean_not float_= false float_= false boolean_= true float_- false) boolean_= boolean_= float_-) float_negative boolean_or float_absolute float_- float_absolute float_=) (float_negative) exec_if (float_- boolean_or boolean_not false float_sqrt in1 float_- exec_dup (float_absolute) float_- boolean_= boolean_= e pi float_positive pi exec_dup (float_sqrt boolean_or boolean_or float_positive false exec_if (float_= float_+ () float_* pi false in1) (float_= float_+ () exec_dup (float_- float_+ (pi boolean_and boolean_and boolean_and float_absolute float_* boolean_not float_negative float_negative exec_dup (float_= e in1 true true float_- float_negative float_absolute) exec_dup (float_- e float_* e boolean_not boolean_= float_- float_negative pi e exec_dup (exec_if (float_negative float_negative exec_dup (float_negative float_negative boolean_and false exec_dup (float_absolute float_- float_absolute true e boolean_and float_negative boolean_not true float_negative boolean_not float_negative float_absolute true pi) float_- float_= exec_if (exec_if (exec_if (pi) (boolean_and float_+ (float_= boolean_not float_negative float_positive in1 boolean_not in1) float_- pi exec_if (e exec_dup (float_sqrt float_positive float_positive exec_dup (float_absolute boolean_not exec_if (float_+ (float_* boolean_= boolean_not float_*) e float_- false float_- float_= float_+ (float_negative in1 float_* float_= exec_dup (float_= exec_dup (boolean_or exec_dup (float_= float_negative pi float_absolute true float_sqrt boolean_= boolean_and boolean_and float_absolute true boolean_not float_* pi boolean_or) false boolean_and float_absolute pi exec_dup (float_- float_negative float_+ (exec_dup (float_positive false float_sqrt boolean_not boolean_= boolean_and float_sqrt boolean_or float_sqrt true e float_+ (float_- float_* float_absolute float_* float_* boolean_not in1 true boolean_or boolean_and pi boolean_and false boolean_and) float_absolute boolean_or float_absolute float_= float_absolute float_* float_=) pi false boolean_and float_negative true boolean_and in1 float_+ (exec_dup (boolean_or in1 e boolean_or float_+ (float_negative) float_positive boolean_and true boolean_= float_absolute false float_negative float_- e boolean_not exec_if (in1 float_* in1 pi float_positive float_absolute float_* boolean_and e float_negative boolean_not float_positive pi float_* float_negative exec_if (e boolean_= float_positive boolean_not) () float_* float_absolute exec_if (boolean_and boolean_not boolean_and float_+ (float_absolute pi float_+ (float_sqrt e exec_dup (float_absolute boolean_or boolean_or true false boolean_= exec_if (float_absolute float_sqrt float_sqrt false float_*) (exec_dup (boolean_not float_positive) exec_if (float_absolute float_absolute in1 in1 boolean_or float_absolute float_positive boolean_and float_= float_= boolean_or float_absolute in1 pi float_- float_sqrt) (boolean_= float_+ () e) boolean_= boolean_or exec_dup (boolean_not boolean_and boolean_or boolean_or exec_if (float_negative false float_positive float_sqrt float_sqrt float_positive boolean_not in1 float_- float_* pi float_absolute boolean_= in1 boolean_or e float_negative exec_if (false exec_if (pi boolean_= float_* float_absolute float_* float_negative in1 float_=) (boolean_= float_sqrt float_- float_= true float_sqrt float_absolute float_= true float_positive float_+ (float_* float_absolute float_- exec_if (float_negative boolean_and boolean_not float_negative float_absolute float_- boolean_and boolean_not exec_dup (in1 exec_dup (exec_if (float_sqrt boolean_or float_negative pi true false float_- boolean_= float_positive float_* float_- float_negative float_= float_- boolean_not boolean_not exec_if (float_- exec_dup (float_absolute float_negative true false float_= float_= in1 float_- float_* pi boolean_not boolean_or boolean_not float_* float_= false true float_- boolean_or e float_positive boolean_= boolean_or boolean_not) exec_if (boolean_or boolean_and float_= float_+ (false exec_if (float_negative boolean_= boolean_not boolean_and boolean_not float_absolute boolean_or false float_- float_- float_negative boolean_and false boolean_or boolean_not boolean_and boolean_and boolean_and boolean_or true float_- boolean_or float_negative boolean_or in1 in1 float_sqrt float_sqrt boolean_= float_sqrt true float_negative boolean_and true false boolean_or float_sqrt boolean_and float_sqrt boolean_not true float_+ (boolean_= float_= boolean_not float_positive exec_dup (float_+ (boolean_= float_absolute float_- float_absolute float_+ (boolean_not in1 boolean_= boolean_or float_negative float_- exec_if (true float_negative boolean_not float_- float_absolute exec_dup (boolean_not float_negative)) (e pi float_- e float_negative float_= float_- exec_dup (boolean_and e in1 in1 false float_negative float_+ (float_positive float_absolute float_negative boolean_and exec_dup (true boolean_or float_sqrt) float_- float_absolute float_+ (e float_- boolean_= float_sqrt boolean_or float_negative pi true boolean_not float_- float_positive float_- float_* float_positive boolean_=) boolean_= float_sqrt pi float_= boolean_= float_+ (pi float_+ (boolean_= float_= exec_dup (float_sqrt float_+ (true) false float_absolute float_negative boolean_and exec_if (e) (float_positive float_sqrt boolean_not float_positive false pi boolean_= true float_- float_- boolean_= false float_= false float_sqrt boolean_or boolean_= e boolean_not float_* pi boolean_not boolean_=) float_- float_absolute exec_if (exec_dup (false float_positive exec_dup (float_-)) float_* boolean_and exec_dup (float_= float_positive float_+ (boolean_and pi exec_dup (float_= true true float_negative exec_if (float_+ (exec_if (exec_if (float_negative float_positive) (float_negative pi float_= true float_positive pi false) float_+ (float_* boolean_not boolean_not) true pi true float_* pi float_= float_sqrt false float_negative e e float_negative float_+ (exec_if (float_- float_* false boolean_not float_sqrt true float_= e boolean_not float_+ (exec_if (boolean_and boolean_not boolean_not boolean_not float_negative float_positive float_= true boolean_not) (float_sqrt pi e exec_dup (e boolean_and float_sqrt float_negative float_positive false float_absolute e false float_sqrt float_negative exec_dup (boolean_= pi float_* boolean_and boolean_or in1 pi boolean_not float_sqrt pi boolean_not float_+ (float_absolute float_sqrt float_positive in1) boolean_and float_* float_= false float_* float_+ (float_- true float_+ (in1 float_positive boolean_and in1 true float_sqrt boolean_and float_negative true float_negative e float_* float_negative float_sqrt float_= in1 e float_- float_positive in1 false float_positive exec_if (e float_absolute float_absolute boolean_not boolean_not float_+ (true float_positive float_= float_+ (exec_if (float_positive pi) (boolean_or pi float_sqrt true pi float_= exec_dup (false float_= float_negative in1 e float_= boolean_= float_* in1 in1 float_+ (float_* true) boolean_not boolean_and e e float_positive boolean_= boolean_or float_* boolean_or float_positive) float_= boolean_not float_= e boolean_or float_* true float_negative float_* float_positive) false) boolean_= exec_dup (in1 float_+ (exec_if (float_* pi boolean_and float_negative float_sqrt float_sqrt boolean_and float_+ (boolean_or float_+ (float_positive false boolean_=) float_positive float_+ (float_* boolean_not float_sqrt) boolean_= true float_* false exec_if (pi float_- false pi boolean_not boolean_not float_+ (float_- float_= float_+ (exec_dup (boolean_and in1 pi float_positive float_- exec_dup (false float_absolute in1 float_negative float_- exec_if () (exec_if (exec_if (boolean_or boolean_not boolean_= exec_dup (false float_negative float_= in1 float_* exec_if (boolean_or float_positive exec_dup (exec_dup (false float_- float_sqrt float_= false float_positive float_- boolean_= float_positive pi boolean_and float_negative float_negative pi boolean_and boolean_not exec_if (boolean_and float_* true pi exec_if (float_sqrt float_* boolean_and float_positive false exec_if (float_negative exec_if (boolean_= exec_if (float_- e float_- false true boolean_and float_* exec_if (float_- float_positive true float_= float_= boolean_not float_- e float_positive e boolean_not float_sqrt pi exec_if (true exec_dup (boolean_= pi false boolean_or boolean_or float_= float_positive float_sqrt float_* float_+ (float_sqrt exec_dup () true true) boolean_and float_positive boolean_= float_positive float_- float_positive float_= boolean_or exec_if (float_- float_positive false float_absolute float_sqrt float_negative true float_* exec_if (float_negative float_negative pi float_= pi float_negative float_absolute float_negative float_* float_* exec_if (exec_if (false float_absolute float_* pi boolean_or float_negative boolean_not boolean_or boolean_not in1 true float_= pi float_sqrt boolean_and e boolean_and boolean_not exec_dup (boolean_= true boolean_or in1 false) in1 e in1 float_= false false boolean_not) (exec_if (float_* true float_= float_positive boolean_not float_- in1 boolean_or float_= float_sqrt float_absolute float_sqrt float_absolute boolean_or true float_negative pi e exec_dup () float_positive float_* in1 boolean_and float_sqrt float_sqrt float_- float_* e float_sqrt) (float_- boolean_= float_+ (float_+ () boolean_= float_+ (float_absolute exec_dup (float_* exec_if (float_absolute boolean_not false boolean_or exec_if (float_positive boolean_and float_negative in1 boolean_or) (boolean_or float_absolute) false float_- pi false boolean_not boolean_and boolean_not float_positive pi boolean_and exec_dup (float_= true float_absolute boolean_= false float_negative float_* pi float_* float_= float_- float_* float_positive in1 boolean_or float_* exec_if (in1 in1 boolean_and exec_if (e float_negative false e float_= boolean_or boolean_and pi float_+ (float_absolute boolean_and pi float_+ (float_* float_= exec_dup (boolean_and false exec_dup (float_- float_positive float_- float_- boolean_= float_sqrt float_+ (float_sqrt boolean_not pi true float_- exec_if (boolean_and float_+ (float_+ (boolean_= e boolean_and float_+ (float_sqrt in1 float_+ (in1 float_sqrt true) float_* float_sqrt pi exec_dup (boolean_and float_sqrt true float_- in1 boolean_and pi true float_positive e false boolean_= float_positive) in1 float_* float_absolute boolean_or false false in1 boolean_and in1 pi float_positive float_= boolean_not) boolean_and float_positive true float_= float_- float_absolute boolean_or boolean_not float_absolute boolean_or in1 float_- float_* e boolean_= exec_if (float_+ (true)) (float_absolute)) float_* boolean_not in1 boolean_or float_sqrt float_- float_+ () exec_if (boolean_not boolean_not exec_if () (exec_dup (boolean_and pi boolean_= float_- exec_dup (true float_negative float_absolute float_* float_*) float_+ (float_* exec_dup (boolean_= e false float_sqrt float_* pi exec_if (float_+ (e float_negative exec_dup (float_+ (boolean_or float_positive true float_* float_absolute float_positive pi boolean_= boolean_not boolean_and boolean_and boolean_not boolean_= float_positive float_negative in1 false exec_dup (float_= float_- boolean_= boolean_and boolean_= float_= float_+ (float_positive float_sqrt) float_* float_* float_negative float_absolute false boolean_= exec_dup (exec_if (false boolean_or float_negative exec_dup (pi) true float_- float_- float_sqrt float_sqrt boolean_or boolean_not exec_if (boolean_and float_* float_* in1 float_- boolean_not float_* float_positive boolean_= float_absolute float_absolute float_sqrt float_- boolean_or boolean_= boolean_or float_= boolean_or e float_* in1 boolean_and false float_= float_+ (float_negative boolean_not float_absolute float_negative boolean_and boolean_and float_+ (boolean_or e boolean_= true in1 float_negative false false float_absolute exec_if () (float_* float_negative float_- exec_if () (pi boolean_not float_- e boolean_or boolean_= float_- float_absolute float_positive float_= float_positive boolean_and boolean_= float_* false boolean_= exec_if (e float_sqrt float_positive float_negative float_= float_sqrt float_positive boolean_and pi pi float_absolute e float_+ (boolean_and boolean_or float_= float_positive float_= boolean_and pi float_negative float_= float_* in1 boolean_and float_absolute float_positive pi float_negative float_* false exec_dup (boolean_not true float_negative false e float_= float_* exec_dup (true false float_positive exec_dup (boolean_and float_negative exec_dup (exec_if (exec_if (boolean_not true float_* false boolean_= pi exec_dup (float_positive float_positive boolean_= float_positive boolean_= in1 boolean_not exec_if (exec_if (float_+ (boolean_or float_positive exec_dup (float_* exec_dup (boolean_not)) in1 boolean_=) exec_dup (e false e boolean_and float_negative float_absolute boolean_and boolean_or in1 boolean_not float_negative exec_dup (boolean_= float_+ (boolean_= pi in1 boolean_= float_positive boolean_= float_= float_sqrt in1 float_* boolean_or boolean_and false true true) float_* exec_if (exec_dup (pi exec_dup (pi float_* in1 exec_dup (float_* e) e exec_dup (exec_dup (false boolean_not boolean_or in1 pi) float_+ (float_negative float_=)) boolean_not exec_if (false exec_dup (exec_dup (float_= float_sqrt true e float_sqrt in1 float_negative pi float_* float_= boolean_= float_* float_- true exec_if (boolean_or float_+ (boolean_or in1) float_- boolean_and boolean_and) (float_absolute float_+ () boolean_= float_absolute e float_absolute exec_dup (float_negative boolean_and exec_if () (float_sqrt) float_positive false float_- e float_positive float_- float_absolute float_+ (float_- boolean_not float_* boolean_and boolean_not boolean_or float_* float_+ (float_* float_- exec_if (float_absolute boolean_not float_* float_sqrt boolean_and e float_= true true boolean_or float_absolute exec_if (float_absolute float_positive pi boolean_= false e false) (pi float_- in1 boolean_= boolean_= float_- float_= float_positive float_* e float_+ (false float_positive exec_if (float_* float_= exec_dup (boolean_and float_absolute float_positive e float_sqrt float_sqrt float_= float_+ (boolean_and exec_dup (float_- boolean_not float_+ (boolean_not float_* boolean_and float_absolute float_= float_sqrt float_absolute false float_+ (true float_absolute exec_if (float_* float_= boolean_or float_= boolean_not in1 float_+ (exec_dup (false exec_dup (e float_= float_- float_= exec_dup (pi float_= float_negative float_positive exec_if (float_* e) (exec_dup (float_- boolean_and false float_- float_* pi e float_sqrt e) float_sqrt) boolean_or float_+ (boolean_not float_* boolean_or float_* false pi exec_if (float_sqrt float_sqrt true float_absolute float_sqrt true exec_dup (float_* float_absolute float_sqrt float_- float_- float_sqrt in1 boolean_= exec_if () (float_negative boolean_or pi float_* float_= boolean_not float_absolute boolean_not e float_* in1) float_sqrt float_+ (float_- false float_= boolean_not e float_- float_sqrt exec_dup (e pi float_absolute float_absolute float_absolute false boolean_and float_- float_negative false boolean_= exec_if (float_- boolean_and float_- false boolean_and in1 boolean_not float_+ (boolean_or float_absolute float_negative e float_- float_+ (exec_dup (pi float_sqrt in1 float_* exec_dup (false float_absolute in1 boolean_= float_=) true in1) float_- float_absolute float_positive) float_= float_= pi exec_if (pi false float_sqrt boolean_or exec_if () (float_absolute float_negative float_- e) float_positive float_absolute boolean_and e float_* float_+ (float_absolute float_- float_* float_positive float_absolute in1 exec_dup (exec_dup (float_positive float_* exec_dup (pi exec_dup (float_- float_absolute float_* float_- boolean_or float_= exec_dup (boolean_or float_- pi float_positive float_* false float_negative float_negative true float_positive float_positive boolean_and e exec_dup (e false)))))))) ())) ())))) ())))))) ())))))) ()))) ()))))))) ()))) ()))) ()) ())) ()) ())))))) ()))))) ()) ())))))) ()))))) ())) ())))))) ()) ())) ())))))) ()) ()) ())) ()) ()) ()) ()) ()) ()) ()))) ())) ()) ())))))) ())) ())))) ()))))))) ())) ())) ())))) ()))))))))))) ())) ()) ()) ()))) ()))) ()) ())))))) ()) ())))))))) ()))) ())) ()) ())) ()))))))) ()))) ()))) ()) ())) ()) ()))))) ()))) ()))))))) ())))) ()) ()))))) ()) ())))) ()) ()) ())) ())))) ()) ()) ())) ()))) ()) ()) ()) ()) ()) ()))))) ()) ())) ()))) ()))) ()))) ()))) ()) ())))) ())))) ()) ())))))))))) ()))))) ())) ()))) ()))) ()) ()) ()))) ()) ()))))) ()))) ()))))))))) ()) ()) ())))) ()) ())) ()))) ()) ())) ()))) ()))) ()))))) ())))))))))) ())) ()) ())) ()) ()) ())
;;; Best total error: 707.1825982984155
;;; Best errors: (91.98299999907613 42.586700439453125 1.722900390625 49.197898864746094 49.55940246582031 64.98000000044703 50.25309753417969 1.17340087890625 48.83290100097656 64.98200000077486 50.16130065917969 0.48670196533203125 4.805896759033203 47.3682975769043 64.98100000061095 15.99100000038743 27.762901306152344 28.099998474121094 2.254199981689453)
;;; Best behaviors: (0.017 45.4133 40.2771 40.8021 40.440598 0.02 39.746902 43.1734 41.1671 0.018 39.8387 42.486702 46.805897 42.631702 0.019 0.009 39.2371 38.9 39.7458)
;;; 
;;; 
;; <-

;; @@

;; @@

;; @@

;; @@
