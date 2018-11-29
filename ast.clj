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
    ;'float_negative
    ;'float_positive
    ;'float_absolute
    ;'float_sqrt
    ;'float_cbrt
    'float_+
    'float_-
    'float_*
    ;'float_%
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
                         #(float (Math/sqrt %))
                         [:float]
                         :float))

(defn float_cbrt
  [state]
  (make-push-instruction state
                         #(float (Math/cbrt %))
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
           :total-error (apply +' errors))))



(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'ast)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :error-function regression-error-function
                                  :max-generations 30
                                  :population-size 200
                                  :max-initial-plushy-size 50
                                  :step-limit 100
                                  :parent-selection :tournament
                                  :tournament-size 5
                                  :mutation-rate 0.05
                                  :crossover :uniform-crossover
                                  :bit-mutation false}
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
;;; Starting GP with args: {:max-initial-plushy-size 50, :bit-mutation false, :crossover :uniform-crossover, :mutation-rate 0.05, :instructions (in1 exec_dup exec_if boolean_and boolean_or boolean_not boolean_= close true false pi e float_+ float_- float_* float_=), :max-generations 30, :parent-selection :tournament, :tournament-size 5, :step-limit 100, :error-function #function[ast/regression-error-function], :population-size 200}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (in1 false float_+ float_+)
;;; Best program: (in1 false float_+ (float_+ ()))
;;; Best total error: 705.8170021073893
;;; Best errors: (91.98299999907613 42.33150041010231 1.707200299948454 48.045598833821714 49.54180072620511 64.98000000044703 50.22269854135811 1.183400969952345 48.496901432052255 64.98200000077486 50.15090062841773 0.7150015123188496 5.339398453012109 47.08559916168451 64.98100000061095 15.99100000038743 27.75350176449865 28.08019893243909 2.2463004402816296)
;;; Best behaviors: (0.017000000923871994 45.66849958989769 40.292799700051546 41.954401166178286 40.45819927379489 0.019999999552965164 39.77730145864189 43.183400969952345 41.503098567947745 0.017999999225139618 39.84909937158227 42.71500151231885 47.33939845301211 42.91440083831549 0.01899999938905239 0.008999999612569809 39.24649823550135 38.91980106756091 39.75369955971837)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_+ float_+)
;;; Best program: (in1 float_+ (float_+ ()))
;;; Best total error: 705.8170021073893
;;; Best errors: (91.98299999907613 42.33150041010231 1.707200299948454 48.045598833821714 49.54180072620511 64.98000000044703 50.22269854135811 1.183400969952345 48.496901432052255 64.98200000077486 50.15090062841773 0.7150015123188496 5.339398453012109 47.08559916168451 64.98100000061095 15.99100000038743 27.75350176449865 28.08019893243909 2.2463004402816296)
;;; Best behaviors: (0.017000000923871994 45.66849958989769 40.292799700051546 41.954401166178286 40.45819927379489 0.019999999552965164 39.77730145864189 43.183400969952345 41.503098567947745 0.017999999225139618 39.84909937158227 42.71500151231885 47.33939845301211 42.91440083831549 0.01899999938905239 0.008999999612569809 39.24649823550135 38.91980106756091 39.75369955971837)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: (boolean_not e false boolean_= float_+ exec_if true in1 false close float_* boolean_= exec_if exec_dup boolean_and exec_dup e boolean_and float_+ close pi boolean_= float_+ true e float_+)
;;; Best program: (boolean_not e false boolean_= float_+ (exec_if (true in1 false) (float_* boolean_= exec_if (exec_dup (boolean_and exec_dup (e boolean_and float_+ () pi boolean_= float_+ (true e float_+ ())))) ())))
;;; Best total error: 613.4745798016602
;;; Best errors: (54.96909292950942 50.96909292950942 4.969092929509422 52.96909292950942 52.96909292950942 27.969092929509422 52.96909292950942 4.969092929509422 52.96909292950942 27.969092929509422 52.96909292950942 4.969092929509422 4.969092929509422 52.96909292950942 27.969092929509422 21.030907070490578 29.969092929509422 29.969092929509422 4.969092929509422)
;;; Best behaviors: (37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058 37.03090707049058)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: (boolean_not e false boolean_= float_+ exec_if float_= true in1 false close float_* boolean_= exec_if exec_dup boolean_and exec_dup e boolean_and exec_dup float_+ close pi boolean_= true float_+ true e float_+)
;;; Best program: (boolean_not e false boolean_= float_+ (exec_if (float_= true in1 false) (float_* boolean_= exec_if (exec_dup (boolean_and exec_dup (e boolean_and exec_dup (float_+ () pi boolean_= true float_+ (true e float_+ ()))))) ())))
;;; Best total error: 519.8013533563089
;;; Best errors: (48.685907622329836 44.685907622329836 1.314092377670164 46.685907622329836 46.685907622329836 21.685907622329836 46.685907622329836 1.314092377670164 46.685907622329836 21.685907622329836 46.685907622329836 1.314092377670164 1.314092377670164 46.685907622329836 21.685907622329836 27.314092377670164 23.685907622329836 23.685907622329836 1.314092377670164)
;;; Best behaviors: (43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164 43.314092377670164)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: (float_* exec_if pi boolean_or exec_if e exec_dup float_* float_- float_+ boolean_and float_- exec_if float_* e true float_* pi false float_+ boolean_= exec_if float_- exec_if close float_*)
;;; Best program: (float_* exec_if (pi boolean_or exec_if (e exec_dup (float_* float_- float_+ (boolean_and float_- exec_if (float_* e true float_* pi false float_+ (boolean_= exec_if (float_- exec_if () (float_*)) ())) ()))) ()) ())
;;; Best total error: 389.3457063526689
;;; Best errors: (17.21809788244373 13.218097882443729 32.78190211755627 15.218097882443729 15.218097882443729 9.781902117556271 15.218097882443729 32.78190211755627 15.218097882443729 9.781902117556271 15.218097882443729 32.78190211755627 32.78190211755627 15.218097882443729 9.781902117556271 58.78190211755627 7.781902117556271 7.781902117556271 32.78190211755627)
;;; Best behaviors: (74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: (float_* exec_if pi boolean_or exec_if e exec_dup float_* float_- float_+ boolean_and float_- exec_if float_* e true float_* pi false float_+ boolean_= exec_if float_- exec_if close float_*)
;;; Best program: (float_* exec_if (pi boolean_or exec_if (e exec_dup (float_* float_- float_+ (boolean_and float_- exec_if (float_* e true float_* pi false float_+ (boolean_= exec_if (float_- exec_if () (float_*)) ())) ()))) ()) ())
;;; Best total error: 389.3457063526689
;;; Best errors: (17.21809788244373 13.218097882443729 32.78190211755627 15.218097882443729 15.218097882443729 9.781902117556271 15.218097882443729 32.78190211755627 15.218097882443729 9.781902117556271 15.218097882443729 32.78190211755627 32.78190211755627 15.218097882443729 9.781902117556271 58.78190211755627 7.781902117556271 7.781902117556271 32.78190211755627)
;;; Best behaviors: (74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: (float_* exec_if pi boolean_or exec_if e exec_dup float_* float_+ boolean_and float_- exec_if float_* e true float_* pi false float_+ boolean_= float_- exec_if close float_*)
;;; Best program: (float_* exec_if (pi boolean_or exec_if (e exec_dup (float_* float_+ (boolean_and float_- exec_if (float_* e true float_* pi false float_+ (boolean_= float_- exec_if () (float_*))) ()))) ()) ())
;;; Best total error: 389.3457063526689
;;; Best errors: (17.21809788244373 13.218097882443729 32.78190211755627 15.218097882443729 15.218097882443729 9.781902117556271 15.218097882443729 32.78190211755627 15.218097882443729 9.781902117556271 15.218097882443729 32.78190211755627 32.78190211755627 15.218097882443729 9.781902117556271 58.78190211755627 7.781902117556271 7.781902117556271 32.78190211755627)
;;; Best behaviors: (74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 7
;;; -------------------------------------------------------
;;; Best plushy: (float_* exec_if pi boolean_or exec_if e exec_dup float_* float_+ boolean_and float_- exec_if float_* e true float_* pi false float_+ boolean_= float_- exec_if close float_*)
;;; Best program: (float_* exec_if (pi boolean_or exec_if (e exec_dup (float_* float_+ (boolean_and float_- exec_if (float_* e true float_* pi false float_+ (boolean_= float_- exec_if () (float_*))) ()))) ()) ())
;;; Best total error: 389.3457063526689
;;; Best errors: (17.21809788244373 13.218097882443729 32.78190211755627 15.218097882443729 15.218097882443729 9.781902117556271 15.218097882443729 32.78190211755627 15.218097882443729 9.781902117556271 15.218097882443729 32.78190211755627 32.78190211755627 15.218097882443729 9.781902117556271 58.78190211755627 7.781902117556271 7.781902117556271 32.78190211755627)
;;; Best behaviors: (74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627 74.78190211755627)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; -------------------------------------------------------
;;; Best plushy: (exec_if float_* false float_- boolean_= e exec_dup true true true boolean_not boolean_not boolean_= boolean_= true float_* float_= exec_if pi float_+ boolean_= close float_* pi boolean_= exec_if exec_if boolean_and exec_dup close exec_if float_* pi float_+ false true close)
;;; Best program: (exec_if (float_* false float_- boolean_= e exec_dup (true true true boolean_not boolean_not boolean_= boolean_= true float_* float_= exec_if (pi float_+ (boolean_=) float_* pi boolean_= exec_if (exec_if (boolean_and exec_dup () exec_if (float_* pi float_+ (false true)) ()) ()) ()) ())) ())
;;; Best total error: 377.53752009761865
;;; Best errors: (21.154159967460416 17.154159967460416 28.845840032539584 19.154159967460416 19.154159967460416 5.845840032539584 19.154159967460416 28.845840032539584 19.154159967460416 5.845840032539584 19.154159967460416 28.845840032539584 28.845840032539584 19.154159967460416 5.845840032539584 54.845840032539584 3.8458400325395843 3.8458400325395843 28.845840032539584)
;;; Best behaviors: (70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 9
;;; -------------------------------------------------------
;;; Best plushy: (exec_if float_* false float_- boolean_= e exec_dup true true true boolean_not boolean_not boolean_= boolean_= true float_* float_= exec_if pi float_+ boolean_= close float_* pi boolean_= exec_if exec_if boolean_and exec_dup close exec_if float_* pi float_+ false true close)
;;; Best program: (exec_if (float_* false float_- boolean_= e exec_dup (true true true boolean_not boolean_not boolean_= boolean_= true float_* float_= exec_if (pi float_+ (boolean_=) float_* pi boolean_= exec_if (exec_if (boolean_and exec_dup () exec_if (float_* pi float_+ (false true)) ()) ()) ()) ())) ())
;;; Best total error: 377.53752009761865
;;; Best errors: (21.154159967460416 17.154159967460416 28.845840032539584 19.154159967460416 19.154159967460416 5.845840032539584 19.154159967460416 28.845840032539584 19.154159967460416 5.845840032539584 19.154159967460416 28.845840032539584 28.845840032539584 19.154159967460416 5.845840032539584 54.845840032539584 3.8458400325395843 3.8458400325395843 28.845840032539584)
;;; Best behaviors: (70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 10
;;; -------------------------------------------------------
;;; Best plushy: (exec_if float_* false float_- boolean_= e exec_dup true true true boolean_= boolean_= float_* float_= exec_if pi float_+ boolean_= close float_* pi boolean_= exec_if exec_if boolean_and exec_dup close exec_if float_* pi float_+ false true close)
;;; Best program: (exec_if (float_* false float_- boolean_= e exec_dup (true true true boolean_= boolean_= float_* float_= exec_if (pi float_+ (boolean_=) float_* pi boolean_= exec_if (exec_if (boolean_and exec_dup () exec_if (float_* pi float_+ (false true)) ()) ()) ()) ())) ())
;;; Best total error: 377.53752009761865
;;; Best errors: (21.154159967460416 17.154159967460416 28.845840032539584 19.154159967460416 19.154159967460416 5.845840032539584 19.154159967460416 28.845840032539584 19.154159967460416 5.845840032539584 19.154159967460416 28.845840032539584 28.845840032539584 19.154159967460416 5.845840032539584 54.845840032539584 3.8458400325395843 3.8458400325395843 28.845840032539584)
;;; Best behaviors: (70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958 70.84584003253958)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 11
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_* pi boolean_or exec_if float_* exec_dup float_* true boolean_and boolean_or boolean_= boolean_= e true exec_if float_+ boolean_= exec_if float_+ boolean_= exec_if true close exec_if exec_if float_* pi float_*)
;;; Best program: (float_* float_* pi boolean_or exec_if (float_* exec_dup (float_* true boolean_and boolean_or boolean_= boolean_= e true exec_if (float_+ (boolean_= exec_if (float_+ (boolean_= exec_if (true) (exec_if (exec_if (float_* pi float_*) ()) ()))) ())) ())) ())
;;; Best total error: 366.62562279946604
;;; Best errors: (25.625622799466 21.625622799466 24.374377200534 23.625622799466 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 24.374377200534 23.625622799466 1.3743772005339991 50.374377200534 0.6256227994660009 0.6256227994660009 24.374377200534)
;;; Best behaviors: (66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 12
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_* pi boolean_or exec_if float_* exec_dup float_* true boolean_and boolean_or boolean_= boolean_= e true exec_if boolean_= exec_if float_+ boolean_= exec_if true close exec_if exec_if float_* pi float_*)
;;; Best program: (float_* float_* pi boolean_or exec_if (float_* exec_dup (float_* true boolean_and boolean_or boolean_= boolean_= e true exec_if (boolean_= exec_if (float_+ (boolean_= exec_if (true) (exec_if (exec_if (float_* pi float_*) ()) ()))) ()) ())) ())
;;; Best total error: 366.62562279946604
;;; Best errors: (25.625622799466 21.625622799466 24.374377200534 23.625622799466 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 24.374377200534 23.625622799466 1.3743772005339991 50.374377200534 0.6256227994660009 0.6256227994660009 24.374377200534)
;;; Best behaviors: (66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 13
;;; -------------------------------------------------------
;;; Best plushy: (false pi float_= boolean_or exec_if exec_dup close true boolean_and float_- close boolean_= exec_dup e float_* exec_dup float_+ float_+ e true float_+ exec_if boolean_or exec_if pi float_+)
;;; Best program: (false pi float_= boolean_or exec_if (exec_dup () true boolean_and float_-) (boolean_= exec_dup (e float_* exec_dup (float_+ (float_+ (e true float_+ (exec_if (boolean_or exec_if (pi float_+ ()) ()) ())))))))
;;; Best total error: 366.2092660353305
;;; Best errors: (25.209266035330515 21.209266035330515 24.790733964669485 23.209266035330515 23.209266035330515 1.790733964669485 23.209266035330515 24.790733964669485 23.209266035330515 1.790733964669485 23.209266035330515 24.790733964669485 24.790733964669485 23.209266035330515 1.790733964669485 50.790733964669485 0.2092660353305149 0.2092660353305149 24.790733964669485)
;;; Best behaviors: (66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949 66.79073396466949)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 14
;;; -------------------------------------------------------
;;; Best plushy: (exec_if float_* pi boolean_or exec_if float_* exec_dup float_* true boolean_and boolean_or boolean_= boolean_= e true boolean_= boolean_= exec_if float_+ boolean_= pi exec_if true exec_if float_*)
;;; Best program: (exec_if (float_* pi boolean_or exec_if (float_* exec_dup (float_* true boolean_and boolean_or boolean_= boolean_= e true boolean_= boolean_= exec_if (float_+ (boolean_= pi exec_if (true exec_if (float_*) ()) ())) ())) ()) ())
;;; Best total error: 366.62562279946604
;;; Best errors: (25.625622799466 21.625622799466 24.374377200534 23.625622799466 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 24.374377200534 23.625622799466 1.3743772005339991 50.374377200534 0.6256227994660009 0.6256227994660009 24.374377200534)
;;; Best behaviors: (66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 15
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_* pi boolean_or exec_if float_* exec_dup float_* true boolean_and boolean_or boolean_= boolean_= e true exec_if boolean_= exec_if float_+ boolean_= exec_if true close exec_if exec_if float_* pi float_*)
;;; Best program: (float_* float_* pi boolean_or exec_if (float_* exec_dup (float_* true boolean_and boolean_or boolean_= boolean_= e true exec_if (boolean_= exec_if (float_+ (boolean_= exec_if (true) (exec_if (exec_if (float_* pi float_*) ()) ()))) ()) ())) ())
;;; Best total error: 366.62562279946604
;;; Best errors: (25.625622799466 21.625622799466 24.374377200534 23.625622799466 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 24.374377200534 23.625622799466 1.3743772005339991 50.374377200534 0.6256227994660009 0.6256227994660009 24.374377200534)
;;; Best behaviors: (66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 16
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_* pi boolean_or exec_if float_* exec_dup float_* true boolean_and boolean_or boolean_= boolean_= e true exec_if boolean_= boolean_= exec_if float_+ boolean_= exec_if true close exec_if exec_if float_* pi float_*)
;;; Best program: (float_* float_* pi boolean_or exec_if (float_* exec_dup (float_* true boolean_and boolean_or boolean_= boolean_= e true exec_if (boolean_= boolean_= exec_if (float_+ (boolean_= exec_if (true) (exec_if (exec_if (float_* pi float_*) ()) ()))) ()) ())) ())
;;; Best total error: 366.62562279946604
;;; Best errors: (25.625622799466 21.625622799466 24.374377200534 23.625622799466 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 24.374377200534 23.625622799466 1.3743772005339991 50.374377200534 0.6256227994660009 0.6256227994660009 24.374377200534)
;;; Best behaviors: (66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 17
;;; -------------------------------------------------------
;;; Best plushy: (float_* float_* pi boolean_or exec_if float_* exec_dup true boolean_and boolean_or boolean_= e true exec_if boolean_= boolean_= exec_if float_+ boolean_= exec_if true close exec_if exec_if float_* pi float_*)
;;; Best program: (float_* float_* pi boolean_or exec_if (float_* exec_dup (true boolean_and boolean_or boolean_= e true exec_if (boolean_= boolean_= exec_if (float_+ (boolean_= exec_if (true) (exec_if (exec_if (float_* pi float_*) ()) ()))) ()) ())) ())
;;; Best total error: 366.62562279946604
;;; Best errors: (25.625622799466 21.625622799466 24.374377200534 23.625622799466 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 23.625622799466 1.3743772005339991 23.625622799466 24.374377200534 24.374377200534 23.625622799466 1.3743772005339991 50.374377200534 0.6256227994660009 0.6256227994660009 24.374377200534)
;;; Best behaviors: (66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534 66.374377200534)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 18
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if exec_if pi boolean_or exec_if boolean_or e float_* float_+ boolean_and float_- float_- exec_if e e float_* float_* pi false float_+ exec_if close exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (exec_if (pi boolean_or exec_if (boolean_or e float_* float_+ (boolean_and float_- float_- exec_if (e e float_* float_* pi false float_+ (exec_if () (exec_if (float_*) ()))) ())) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 19
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if exec_if pi boolean_or exec_if e float_* float_+ boolean_and float_- float_- exec_if e e float_* float_* pi false float_+ exec_if exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (exec_if (pi boolean_or exec_if (e float_* float_+ (boolean_and float_- float_- exec_if (e e float_* float_* pi false float_+ (exec_if (exec_if (float_*) ()) ())) ())) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 20
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if exec_if pi boolean_or exec_if boolean_or e float_* float_+ boolean_and float_- float_- exec_if e e float_* float_* pi false float_+ close exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (exec_if (pi boolean_or exec_if (boolean_or e float_* float_+ (boolean_and float_- float_- exec_if (e e float_* float_* pi false float_+ () exec_if (float_*) ()) ())) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 21
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if exec_if pi boolean_and exec_if e float_* float_+ boolean_and float_- float_- exec_if e e float_* float_* pi false float_+ exec_if exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (exec_if (pi boolean_and exec_if (e float_* float_+ (boolean_and float_- float_- exec_if (e e float_* float_* pi false float_+ (exec_if (exec_if (float_*) ()) ())) ())) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 22
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_not boolean_or exec_if exec_if pi boolean_or exec_if boolean_or e float_* float_+ boolean_and float_- close float_- exec_if e e float_* float_* pi false float_+ close exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_not boolean_or exec_if (exec_if (pi boolean_or exec_if (boolean_or e float_* float_+ (boolean_and float_-) float_- exec_if (e e float_* float_* pi false float_+ () exec_if (float_*) ()) ()) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 23
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if exec_if pi exec_if e float_* float_+ boolean_and float_- float_- exec_if e e float_* float_* pi false float_+ exec_if exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (exec_if (pi exec_if (e float_* float_+ (boolean_and float_- float_- exec_if (e e float_* float_* pi false float_+ (exec_if (exec_if (float_*) ()) ())) ())) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 24
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_not boolean_or exec_if exec_if pi boolean_or exec_if boolean_or e float_* float_+ boolean_and float_- close float_- e e float_* float_* pi false float_+ close exec_if close)
;;; Best program: (in1 float_* float_* boolean_not boolean_or exec_if (exec_if (pi boolean_or exec_if (boolean_or e float_* float_+ (boolean_and float_-) float_- e e float_* float_* pi false float_+ () exec_if () ()) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 25
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if exec_if pi boolean_and exec_if e float_* float_+ boolean_and float_- float_- exec_if exec_if e e float_* float_* pi false float_+ exec_if exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (exec_if (pi boolean_and exec_if (e float_* float_+ (boolean_and float_- float_- exec_if (exec_if (e e float_* float_* pi false float_+ (exec_if (exec_if (float_*) ()) ())) ()) ())) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 26
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if pi boolean_and exec_if e float_* float_+ boolean_and float_- float_- exec_if exec_if e e float_* float_* pi false float_+ exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (pi boolean_and exec_if (e float_* float_+ (boolean_and float_- float_- exec_if (exec_if (e e float_* float_* pi false float_+ (exec_if (float_*) ())) ()) ())) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 27
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if pi boolean_and exec_if e float_* float_+ boolean_= boolean_and float_- float_- exec_if exec_if e e float_* float_* pi false float_+ exec_if exec_if float_* boolean_not close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (pi boolean_and exec_if (e float_* float_+ (boolean_= boolean_and float_- float_- exec_if (exec_if (e e float_* float_* pi false float_+ (exec_if (exec_if (float_* boolean_not) ()) ())) ()) ())) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 28
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if exec_if pi boolean_and exec_if e float_* float_+ boolean_and float_- float_- exec_if exec_if e e float_* float_* pi false float_+ exec_if exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (exec_if (pi boolean_and exec_if (e float_* float_+ (boolean_and float_- float_- exec_if (exec_if (e e float_* float_* pi false float_+ (exec_if (exec_if (float_*) ()) ())) ()) ())) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 29
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if exec_if pi boolean_and exec_if e float_* float_+ boolean_and float_- float_- exec_if exec_if e e float_* float_* pi false float_+ exec_if exec_if float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (exec_if (pi boolean_and exec_if (e float_* float_+ (boolean_and float_- float_- exec_if (exec_if (e e float_* float_* pi false float_+ (exec_if (exec_if (float_*) ()) ())) ()) ())) ()) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 30
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_* boolean_or exec_if pi boolean_and exec_if e float_* float_+ boolean_and float_- float_- exec_if exec_if e e float_* float_* pi false float_+ exec_if exec_if float_* float_* close)
;;; Best program: (in1 float_* float_* boolean_or exec_if (pi boolean_and exec_if (e float_* float_+ (boolean_and float_- float_- exec_if (exec_if (e e float_* float_* pi false float_+ (exec_if (exec_if (float_* float_*) ()) ())) ()) ())) ()) ())
;;; Best total error: 341.957578118314
;;; Best errors: (25.757832105117302 19.744212736656863 24.383095144655314 21.160893900459627 23.98710001832842 1.242167894882698 23.934084493656457 24.193119497411573 21.104252159498444 1.242167894882698 24.020145418487502 22.930632312616197 7.53001515890594 23.661496456457897 1.242167894882698 50.2421678948827 0.6516305799995195 0.7955422773908793 24.134854279141337)
;;; Best behaviors: (66.2421678948827 68.25578726334314 66.38309514465531 68.83910609954037 66.01289998167158 66.2421678948827 66.06591550634354 66.19311949741157 68.89574784050156 66.2421678948827 65.9798545815125 64.9306323126162 49.53001515890594 66.3385035435421 66.2421678948827 66.2421678948827 66.34836942000048 66.20445772260912 66.13485427914134)
;;; 
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@


;; @@

;; @@

;; @@

;; @@

;; @@
