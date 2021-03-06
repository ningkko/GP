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
  "gets a small sample of input"
  [filename]
  (let [raw-input (fn[row-num] (drop-last (read-row filename row-num)))
        first-floats (fn[row-num] (take 4 (raw-input row-num)))
       	boolean-input (fn[row-num] (take-last 7 (raw-input row-num)))
        second-floats (fn[row-num] (rest (take-last 7 (raw-input row-num))))
        one-raw (fn[row-num] (flatten (concat (map #(to-float %) (first-floats row-num))
                              (list (boolean (boolean-input row-num)))
                              (map #(to-float %) (second-floats row-num)))))]
    (rest (map one-raw (range (count (read-column filename 0)))))))
  

  
(defn get-target
  "gets all target from the file"
  [file-name]
  (doall
    (map #(float (read-string %))
         (rest (read-column file-name 11)))))


(defn get-sample-input
  "gets a small sample of input"
  [filename size]
  (let [raw-input (fn[row-num] (drop-last (read-row filename row-num)))
        first-floats (fn[row-num] (take 4 (raw-input row-num)))
       	boolean-input (fn[row-num] (take-last 7 (raw-input row-num)))
        second-floats (fn[row-num] (rest (take-last 7 (raw-input row-num))))
        one-raw (fn[row-num] (flatten (concat (map #(to-float %) (first-floats row-num))
                              (list (boolean (boolean-input row-num)))
                              (map #(to-float %) (second-floats row-num)))))]
    (rest(map one-raw (range size)))))


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

(def target-types (rest(distinct (read-column data-addr 11))))
(def target-type-nums (count target-types))
target-types
target-type-nums


;; target types after taking modulo
(def packed-target (map #(float %)
                              (map #(cond
                                      (= % (float 92)) 1
                                      (= % (float 88)) 2
                                      (= % (float 42)) 3
                                      (= % (float 90)) 4
                                      (= % (float 65)) 5
                                      (= % (float 16)) 6
                                      (= % (float 67)) 7
                                      (= % (float 95)) 8
                                      (= % (float 62)) 9
                                      (= % (float 15)) 10
                                      (= % (float 52)) 11
                                      (= % (float 6)) 12
                                      (= % (float 64)) 13
                                      (= % (float 53)) 14) target)))

target
packed-target

;;input
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>1.0</span>","value":"1.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>7.0</span>","value":"7.0"},{"type":"html","content":"<span class='clj-unkown'>7.0</span>","value":"7.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>8.0</span>","value":"8.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>9.0</span>","value":"9.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>10.0</span>","value":"10.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>9.0</span>","value":"9.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>8.0</span>","value":"8.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>11.0</span>","value":"11.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>9.0</span>","value":"9.0"},{"type":"html","content":"<span class='clj-unkown'>11.0</span>","value":"11.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>1.0</span>","value":"1.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>11.0</span>","value":"11.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>11.0</span>","value":"11.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-unkown'>8.0</span>","value":"8.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>11.0</span>","value":"11.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"},{"type":"html","content":"<span class='clj-unkown'>11.0</span>","value":"11.0"},{"type":"html","content":"<span class='clj-unkown'>4.0</span>","value":"4.0"}],"value":"(1.0 2.0 3.0 4.0 4.0 5.0 4.0 3.0 4.0 5.0 4.0 3.0 3.0 4.0 5.0 6.0 7.0 7.0 3.0 8.0 2.0 9.0 2.0 3.0 6.0 10.0 3.0 4.0 4.0 4.0 3.0 4.0 5.0 4.0 4.0 3.0 4.0 2.0 2.0 6.0 4.0 9.0 6.0 4.0 5.0 4.0 6.0 5.0 5.0 2.0 4.0 5.0 3.0 5.0 8.0 4.0 5.0 4.0 2.0 11.0 5.0 3.0 6.0 3.0 4.0 9.0 11.0 5.0 4.0 1.0 5.0 4.0 11.0 4.0 4.0 4.0 4.0 5.0 5.0 11.0 4.0 2.0 8.0 4.0 4.0 4.0 4.0 4.0 5.0 6.0 4.0 6.0 11.0 4.0 4.0 6.0 6.0 4.0 11.0 4.0)"}
;; <=

;; @@
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def default-instructions
  (list
	'in1
    'in1
    'in1
    'in1
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
   	'float_log
   	'float_sin
   	'float_cos
   	'float_tan
   	'float_sinh
   	'float_cosh
   	'float_tanh
   	'float_ceil
   	'float_floor
   	'float_round
   	'random-coefficient
   	
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
      (let [args (:args args-pop-result)
            result (apply function args)
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


(defn float_log
  [state]
  (make-push-instruction state
                         #(float (Math/log %))
                         [:float]
                         [:float]))


(defn float_cos
  [state]
  (make-push-instruction state
                         #(float (Math/cos %))
                         [:float]
                         [:float]))


(defn float_sin
  [state]
  (make-push-instruction state
                         #(float (Math/sin %))
                         [:float]
                         [:float]))



(defn float_tan
  [state]
  (make-push-instruction state
                         #(float (Math/tan %))
                         [:float]
                         [:float]))



(defn float_sinh
  [state]
  (make-push-instruction state
                         #(float (Math/sinh %))
                         [:float]
                         [:float]))



(defn float_cosh
  [state]
  (make-push-instruction state
                         #(float (Math/cosh %))
                         [:float]
                         [:float]))



(defn float_tanh
  [state]
  (make-push-instruction state
                         #(float (Math/tanh %))
                         [:float]
                         [:float]))




(defn float_ceil
  [state]
  (make-push-instruction state
                         #(float (Math/ceil %))
                         [:float]
                         [:float]))


(defn float_floor
  [state]
  (make-push-instruction state
                         #(float (Math/floor %))
                         [:float]
                         [:float]))



(defn float_round
  [state]
  (make-push-instruction state
                         #(float (Math/round %))
                         [:float]
                         [:float]))




(defn float_mod
  "see what category the predicted output falls in"
  [state]
  (make-push-instruction state
                         #(float (mod %1 15))
                         [:float]
                         [:float]))









;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/float_mod</span>","value":"#'gp.ast/float_mod"}
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

(defn absolute-badness
  "calculates the absolute difference between a predicted value and a target value"
  [correct-output output]
  (abs (- correct-output output))
)

(defn modulo-badness
  "Take modulo of the predicted outputs"
  [correct-output output]
  (if (= (float (mod (Math/round output) 15)) correct-output)
    0
    1))

(defn regression-error-function
  "Finds the behaviors and errors of the individual."
  [argmap past-total-errors individual]
  (let [program (push-from-plushy (:plushy individual))
        badness-function (:badness-function argmap)
        inputs input
        correct-outputs (cond
                          (= badness-function absolute-badness) target
                          (= badness-function modulo-badness) packed-target)
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
                        
                        ;; if the answer does not contain  in1
                        (not (= nil (some #{:in1} (:plushy individual)))) 
                        (float 2000000)
						(= (rand-nth outputs) (rand-nth outputs)) (float 2000000)
                        :else
                        (badness-function correct-output output)))
                    correct-outputs
                    outputs)]
    
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (let [total-err (apply +' errors)]
                          (cond
                            (Double/isNaN total-err) (float 1000000)
                            ; (= (rand-nth errors) (/ total-err (count errors))) (float 2000000)
                            (not (= nil (some #{total-err} past-total-errors))) (float 3000000)
                            :else total-err)))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/regression-error-function</span>","value":"#'gp.ast/regression-error-function"}
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
         past-total-errors []];; also check to prevent same total-error all the time

    (let [evaluated-pop (sort-by :total-error 

                                 (map (partial error-function argmap past-total-errors)
                                      population))]

      (report evaluated-pop generation (:out-file argmap))
      (cond


        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size #(new-individual evaluated-pop argmap))
                     (conj  past-total-errors (:total-error (first evaluated-pop))))))))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'gp.ast)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :badness-function modulo-badness
                                  :error-function regression-error-function
                                  :max-generations 200
                                  :population-size 200
                                  :max-initial-plushy-size 40
                                  :step-limit 10
                                  :parent-selection :lexicase
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
;;(-main)
;; @@

;; @@

;; @@
