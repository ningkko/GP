;; gorilla-repl.fileformat = 1

;; @@
(ns propel-test
  (:gen-class))

(def example-push-state
  {:exec '()
   :float '(1.0 2.0 3.0 4.0 5.0 6.0 7.0)
   :input {:in1 4}})


; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def instructions
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
   
    pi
    e
    'boolean_negative
    'boolean_positive
    ;;'float_floatify
    'float_absolute
    'float_sqrt
    'float_cbrt
    'float_+
    'float_-
    'float_*
    ;;'float_%
    'float_=
   ))

(def opens ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1 
   'exec_if 2})

;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
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

;;(defn float_floatify
;;  [state]
;;  (make-push-instruction state
;;                         #(float %)
;;                         [:integer]
;;                         :float))


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

;;;;;;;;;
;; GP

(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))

(defn tournament-selection
  "Elements are sorted according to their erropr first and then the first half will be taken. After which 1/10 of them will be selected"
  [pop]
  (let [half-size (/ (count pop) 2)
        tournament-set (take half-size (apply min-key :total-error pop))
        tournament-size (/ (count tournament-set) 10)]
        (take tournament-size (shuffle pop))
    ))



(defn crossover
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



(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) 0.05)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy]
  (remove (fn [x] (< (rand) 0.05))
          plushy))

(defn select-and-vary
  "Selects parent(s) from population and varies them."
  [pop]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (crossover (:plushy (tournament-selection pop))
                               (:plushy (tournament-selection pop)))
       (< prob 0.75) (uniform-addition (:plushy (tournament-selection pop)))
       :else (uniform-deletion (:plushy (tournament-selection pop)))))})

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
                     (repeatedly population-size #(select-and-vary evaluated-pop)))))))

;;;;;;;;;
;; Problem: f(x) = 7x^2 - 20x + 13

(defn target-function-hard
  "Target function: f(x) = 7x^2 - 20x + 13"
  [x]
  (+ (* 7 x x)
     (* -20 x)
     13))

(defn target-function
  "Target function: f(x) = x^3 + x + 3"
  [x]
  (+ (* x x x)
     x
     3))

(defn regression-error-function
  "Finds the behaviors and errors of the individual."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        inputs (doall (map #(float %) (range -10 11)))
        correct-outputs (map target-function inputs)
        outputs (map (fn [input]
                       (peek-stack
                        (interpret-program 
                          program
                          (assoc empty-push-state :input {:in1 input})
                          (:step-limit argmap))
                        :integer))
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
  (binding [*ns* (the-ns 'propel-test)]
    (propel-gp (update-in (merge {:instructions instructions
                                  :error-function regression-error-function
                                  :max-generations 100
                                  :population-size 200
                                  :max-initial-plushy-size 50
                                  :step-limit 100}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))




;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;propel-test/-main</span>","value":"#'propel-test/-main"}
;; <=

;; @@
(-main)
;; @@
;; ->
;;; Starting GP with args: {:instructions (in1 exec_dup exec_if boolean_and boolean_or boolean_not boolean_= close true false 3.141592653589793 2.718281828459045 boolean_negative boolean_positive float_absolute float_sqrt float_cbrt float_+ float_- float_* float_=), :error-function #function[propel-test/regression-error-function], :max-generations 100, :population-size 200, :max-initial-plushy-size 50, :step-limit 100}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (false false boolean_negative boolean_not float_= boolean_or boolean_not true float_absolute true float_+ boolean_= boolean_negative float_cbrt float_cbrt boolean_negative float_= exec_dup 2.718281828459045 float_= in1 false true in1 float_= float_sqrt 3.141592653589793 boolean_positive exec_if boolean_and false boolean_or float_absolute boolean_= 3.141592653589793 false float_= float_= float_+ in1 exec_if 2.718281828459045 float_sqrt close)
;;; Best program: (false false boolean_negative boolean_not float_= boolean_or boolean_not true float_absolute true float_+ boolean_= boolean_negative float_cbrt float_cbrt boolean_negative float_= exec_dup (2.718281828459045 float_= in1 false true in1 float_= float_sqrt 3.141592653589793 boolean_positive exec_if (boolean_and false boolean_or float_absolute boolean_= 3.141592653589793 false float_= float_= float_+ in1 exec_if (2.718281828459045 float_sqrt) ()) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 7
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 9
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 10
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 11
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 12
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 13
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 14
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 15
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 16
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 17
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 18
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 19
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 20
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 21
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 22
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 23
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 24
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 25
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 26
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 27
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 28
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 29
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 30
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 31
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 32
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 33
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 34
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 35
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 36
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 37
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 38
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 39
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 40
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 41
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 42
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 43
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 44
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 45
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 46
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 47
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 48
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 49
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 50
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 51
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 52
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 53
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 54
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 55
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 56
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 57
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 58
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 59
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 60
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 61
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 62
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 63
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 64
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 65
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 66
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 67
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 68
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 69
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 70
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 71
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 72
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 73
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 74
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 75
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 76
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 77
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 78
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 79
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 80
;;; -------------------------------------------------------
;;; Best plushy: (boolean_and)
;;; Best program: (boolean_and)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 81
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 82
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 83
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 84
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 85
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 86
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 87
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 88
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 89
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 90
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 91
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 92
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 93
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 94
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 95
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 96
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 97
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 98
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 99
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 100
;;; -------------------------------------------------------
;;; Best plushy: ()
;;; Best program: ()
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
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
