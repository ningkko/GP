;; gorilla-repl.fileformat = 1

;; @@
(ns propeltest
  (:gen-class))


(def data-addr "src/training_set_metadata.csv")


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
   
    'pi
    'e
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

(defn get-target
  [file-name]
  (doall
    (map #(float (read-string %))
         (rest (read-column file-name 11)))))


(def empty-push-state
  {:exec '()
   :float '()
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
  "Selects an individual for variation using a tournament."
  [pop]
  (let [tournament-size 5
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))


;;================================ lexicase test ==========================================

(defn lexicase-selection
  [population cases]
  (loop [candidates population
         cases cases]
    (if (or (empty? cases)
            (empty? (rest candidates)))
      (rand-nth candidates)
      (let [min-err-for-case (apply min (map #(nth % (first cases)) (map #(:errors %) candidates)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case) candidates)
               (rest cases))))))

;;==========================================================================================

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

;;---------------------------test------------------------------------------------------------
(defn select-and-vary
  "Selects parent(s) from population and varies them."
  [pop]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (crossover (:plushy (lexicase-selection pop cases))
                               (:plushy (lexicase-selection pop cases)))
       (< prob 0.75) (uniform-addition (:plushy (lexicase-selection pop cases)))
       :else (uniform-deletion (:plushy (lexicase-selection pop cases)))))})

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
  (let [cases ]
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
                       (repeatedly population-size #(select-and-vary evaluated-pop cases))))))))

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
        inputs ()
        correct-outputs (get-target data-addr)
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
  (binding [*ns* (the-ns 'propeltest)]
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;propeltest/-main</span>","value":"#'propeltest/-main"}
;; <=

;; @@
(-main)
;; @@
;; ->
;;; Starting GP with args: {:instructions (in1 exec_dup exec_if boolean_and boolean_or boolean_not boolean_= close true false 3.141592653589793 2.718281828459045 boolean_negative boolean_positive float_absolute float_sqrt float_cbrt float_+ float_- float_* float_=), :error-function #function[propeltest/regression-error-function], :max-generations 100, :population-size 200, :max-initial-plushy-size 50, :step-limit 100}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (float_cbrt 3.141592653589793 boolean_positive exec_if 2.718281828459045 boolean_not boolean_and float_+ float_- boolean_or)
;;; Best program: (float_cbrt 3.141592653589793 boolean_positive exec_if (2.718281828459045 boolean_not boolean_and float_+ float_- boolean_or) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: (exec_if float_= exec_dup float_* boolean_and float_* boolean_=)
;;; Best program: (exec_if (float_= exec_dup (float_* boolean_and float_* boolean_=)) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: (true close false float_= boolean_not 3.141592653589793 false boolean_not boolean_positive boolean_not boolean_positive boolean_negative float_+ in1 in1 boolean_not float_+ float_sqrt boolean_positive false boolean_or boolean_= close boolean_not in1 boolean_positive float_*)
;;; Best program: (true false float_= boolean_not 3.141592653589793 false boolean_not boolean_positive boolean_not boolean_positive boolean_negative float_+ in1 in1 boolean_not float_+ float_sqrt boolean_positive false boolean_or boolean_= boolean_not in1 boolean_positive float_*)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: (float_* false 2.718281828459045 2.718281828459045 exec_dup boolean_not in1 boolean_or true exec_dup float_- float_- float_* 2.718281828459045 boolean_= in1 close boolean_negative 2.718281828459045 3.141592653589793 float_* 3.141592653589793 boolean_= float_* boolean_and float_sqrt boolean_not exec_if exec_dup boolean_or)
;;; Best program: (float_* false 2.718281828459045 2.718281828459045 exec_dup (boolean_not in1 boolean_or true exec_dup (float_- float_- float_* 2.718281828459045 boolean_= in1) boolean_negative 2.718281828459045 3.141592653589793 float_* 3.141592653589793 boolean_= float_* boolean_and float_sqrt boolean_not exec_if (exec_dup (boolean_or)) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: (float_= boolean_not exec_if float_+ float_= exec_if boolean_negative float_absolute 3.141592653589793 float_- boolean_or float_sqrt boolean_= 3.141592653589793 3.141592653589793 float_= boolean_not boolean_negative boolean_or boolean_or boolean_or close exec_if 3.141592653589793 exec_dup exec_if)
;;; Best program: (float_= boolean_not exec_if (float_+ float_= exec_if (boolean_negative float_absolute 3.141592653589793 float_- boolean_or float_sqrt boolean_= 3.141592653589793 3.141592653589793 float_= boolean_not boolean_negative boolean_or boolean_or boolean_or) (exec_if (3.141592653589793 exec_dup (exec_if () ())) ())) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: (float_= float_* float_absolute boolean_= float_+ exec_dup true boolean_and float_= boolean_= close float_sqrt 2.718281828459045 false float_absolute boolean_negative 2.718281828459045 float_- float_cbrt float_absolute boolean_positive false 2.718281828459045 float_cbrt close boolean_and float_absolute close)
;;; Best program: (float_= float_* float_absolute boolean_= float_+ exec_dup (true boolean_and float_= boolean_=) float_sqrt 2.718281828459045 false float_absolute boolean_negative 2.718281828459045 float_- float_cbrt float_absolute boolean_positive false 2.718281828459045 float_cbrt boolean_and float_absolute)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: (boolean_not exec_dup float_- boolean_negative 3.141592653589793 float_sqrt 3.141592653589793 float_+ boolean_negative float_* float_= float_= false close exec_if float_+ float_- 3.141592653589793 boolean_positive float_= float_= 3.141592653589793 3.141592653589793 exec_dup boolean_positive float_cbrt boolean_or in1 exec_if boolean_and boolean_= boolean_not)
;;; Best program: (boolean_not exec_dup (float_- boolean_negative 3.141592653589793 float_sqrt 3.141592653589793 float_+ boolean_negative float_* float_= float_= false) exec_if (float_+ float_- 3.141592653589793 boolean_positive float_= float_= 3.141592653589793 3.141592653589793 exec_dup (boolean_positive float_cbrt boolean_or in1 exec_if (boolean_and boolean_= boolean_not) ())) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 7
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt true true close boolean_not boolean_not float_+ 2.718281828459045 float_+ float_absolute float_* float_absolute 2.718281828459045 2.718281828459045 boolean_negative 2.718281828459045 in1 float_- float_* boolean_= float_- float_- false boolean_positive close false false boolean_or boolean_positive float_absolute true boolean_or exec_if boolean_= float_cbrt exec_if)
;;; Best program: (float_sqrt true true boolean_not boolean_not float_+ 2.718281828459045 float_+ float_absolute float_* float_absolute 2.718281828459045 2.718281828459045 boolean_negative 2.718281828459045 in1 float_- float_* boolean_= float_- float_- false boolean_positive false false boolean_or boolean_positive float_absolute true boolean_or exec_if (boolean_= float_cbrt exec_if () ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; -------------------------------------------------------
;;; Best plushy: (boolean_negative float_absolute float_sqrt float_cbrt boolean_= boolean_or float_= 3.141592653589793 close float_sqrt boolean_negative boolean_not boolean_negative float_* 2.718281828459045 boolean_= false 2.718281828459045 false boolean_positive boolean_negative boolean_positive true false)
;;; Best program: (boolean_negative float_absolute float_sqrt float_cbrt boolean_= boolean_or float_= 3.141592653589793 float_sqrt boolean_negative boolean_not boolean_negative float_* 2.718281828459045 boolean_= false 2.718281828459045 false boolean_positive boolean_negative boolean_positive true false)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 9
;;; -------------------------------------------------------
;;; Best plushy: (float_= float_+ in1 boolean_and boolean_not exec_dup boolean_not 2.718281828459045 boolean_positive float_* close float_cbrt boolean_not boolean_positive float_= true float_- boolean_and exec_if boolean_= float_+ boolean_or float_* boolean_negative false 3.141592653589793 boolean_not 3.141592653589793 float_-)
;;; Best program: (float_= float_+ in1 boolean_and boolean_not exec_dup (boolean_not 2.718281828459045 boolean_positive float_*) float_cbrt boolean_not boolean_positive float_= true float_- boolean_and exec_if (boolean_= float_+ boolean_or float_* boolean_negative false 3.141592653589793 boolean_not 3.141592653589793 float_-) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 10
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute float_sqrt boolean_and boolean_and float_* 3.141592653589793 false 2.718281828459045 boolean_or float_absolute 2.718281828459045 float_* exec_dup false false 2.718281828459045 true true 3.141592653589793 false close float_* false boolean_or float_sqrt boolean_or float_absolute exec_if float_absolute boolean_= in1)
;;; Best program: (float_absolute float_sqrt boolean_and boolean_and float_* 3.141592653589793 false 2.718281828459045 boolean_or float_absolute 2.718281828459045 float_* exec_dup (false false 2.718281828459045 true true 3.141592653589793 false) float_* false boolean_or float_sqrt boolean_or float_absolute exec_if (float_absolute boolean_= in1) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 11
;;; -------------------------------------------------------
;;; Best plushy: (boolean_positive in1 float_= float_cbrt float_= boolean_not boolean_not close float_- 2.718281828459045 boolean_or float_* false float_- boolean_positive float_absolute boolean_negative in1 float_absolute 3.141592653589793 true boolean_negative 3.141592653589793 true false)
;;; Best program: (boolean_positive in1 float_= float_cbrt float_= boolean_not boolean_not float_- 2.718281828459045 boolean_or float_* false float_- boolean_positive float_absolute boolean_negative in1 float_absolute 3.141592653589793 true boolean_negative 3.141592653589793 true false)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 12
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute float_absolute exec_if float_= boolean_or boolean_positive exec_if float_absolute exec_dup float_absolute float_+ float_- exec_dup float_absolute close float_+ close boolean_and boolean_negative float_+ boolean_negative)
;;; Best program: (float_absolute float_absolute exec_if (float_= boolean_or boolean_positive exec_if (float_absolute exec_dup (float_absolute float_+ float_- exec_dup (float_absolute) float_+) boolean_and boolean_negative float_+ boolean_negative) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 13
;;; -------------------------------------------------------
;;; Best plushy: (float_* exec_if boolean_and 2.718281828459045 float_* false float_= 2.718281828459045 boolean_or in1 boolean_= float_* exec_dup close float_absolute exec_if boolean_positive 3.141592653589793 boolean_or false float_absolute false boolean_or 2.718281828459045 float_sqrt boolean_and 2.718281828459045 float_= boolean_=)
;;; Best program: (float_* exec_if (boolean_and 2.718281828459045 float_* false float_= 2.718281828459045 boolean_or in1 boolean_= float_* exec_dup () float_absolute exec_if (boolean_positive 3.141592653589793 boolean_or false float_absolute false boolean_or 2.718281828459045 float_sqrt boolean_and 2.718281828459045 float_= boolean_=) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 14
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt float_= true 3.141592653589793 boolean_not boolean_negative false exec_if float_- float_- float_sqrt in1 float_- in1 in1 boolean_negative boolean_or false boolean_positive 2.718281828459045 close 2.718281828459045 false boolean_and boolean_or exec_dup float_absolute)
;;; Best program: (float_sqrt float_= true 3.141592653589793 boolean_not boolean_negative false exec_if (float_- float_- float_sqrt in1 float_- in1 in1 boolean_negative boolean_or false boolean_positive 2.718281828459045) (2.718281828459045 false boolean_and boolean_or exec_dup (float_absolute)))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 15
;;; -------------------------------------------------------
;;; Best plushy: (float_* true exec_if boolean_or in1 float_- float_+ exec_dup in1 float_absolute float_sqrt float_absolute float_absolute true float_+ close float_- 3.141592653589793 float_- close boolean_or boolean_negative boolean_or boolean_= boolean_not boolean_positive float_absolute float_+ true)
;;; Best program: (float_* true exec_if (boolean_or in1 float_- float_+ exec_dup (in1 float_absolute float_sqrt float_absolute float_absolute true float_+) float_- 3.141592653589793 float_-) (boolean_or boolean_negative boolean_or boolean_= boolean_not boolean_positive float_absolute float_+ true))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 16
;;; -------------------------------------------------------
;;; Best plushy: (float_= exec_if true boolean_negative float_cbrt boolean_negative false float_- float_sqrt 2.718281828459045 false boolean_positive float_* 2.718281828459045 boolean_positive boolean_positive float_= boolean_positive float_cbrt boolean_negative float_sqrt 2.718281828459045 boolean_or float_sqrt close)
;;; Best program: (float_= exec_if (true boolean_negative float_cbrt boolean_negative false float_- float_sqrt 2.718281828459045 false boolean_positive float_* 2.718281828459045 boolean_positive boolean_positive float_= boolean_positive float_cbrt boolean_negative float_sqrt 2.718281828459045 boolean_or float_sqrt) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 17
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or float_absolute close boolean_or exec_dup boolean_negative boolean_= boolean_or exec_if float_absolute float_sqrt float_absolute float_absolute float_absolute 3.141592653589793 false boolean_negative float_- true float_absolute float_cbrt boolean_and boolean_negative boolean_or exec_if boolean_not float_absolute in1)
;;; Best program: (boolean_or float_absolute boolean_or exec_dup (boolean_negative boolean_= boolean_or exec_if (float_absolute float_sqrt float_absolute float_absolute float_absolute 3.141592653589793 false boolean_negative float_- true float_absolute float_cbrt boolean_and boolean_negative boolean_or exec_if (boolean_not float_absolute in1) ()) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 18
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or 2.718281828459045 close 3.141592653589793 float_= float_= boolean_negative float_= boolean_or exec_if false float_sqrt boolean_and 2.718281828459045 close 3.141592653589793 false float_+ float_- close float_absolute 3.141592653589793 float_- boolean_negative float_absolute exec_if boolean_not)
;;; Best program: (boolean_or 2.718281828459045 3.141592653589793 float_= float_= boolean_negative float_= boolean_or exec_if (false float_sqrt boolean_and 2.718281828459045) (3.141592653589793 false float_+ float_-) float_absolute 3.141592653589793 float_- boolean_negative float_absolute exec_if (boolean_not) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 19
;;; -------------------------------------------------------
;;; Best plushy: (boolean_not float_sqrt true float_+ exec_dup float_= float_= true float_* close float_absolute float_cbrt boolean_or true boolean_or float_* float_- float_+ false boolean_= false float_* boolean_and 3.141592653589793 3.141592653589793 float_absolute)
;;; Best program: (boolean_not float_sqrt true float_+ exec_dup (float_= float_= true float_*) float_absolute float_cbrt boolean_or true boolean_or float_* float_- float_+ false boolean_= false float_* boolean_and 3.141592653589793 3.141592653589793 float_absolute)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 20
;;; -------------------------------------------------------
;;; Best plushy: (float_cbrt float_+ boolean_and true exec_dup exec_dup false float_+ boolean_= exec_dup false float_absolute float_+ 3.141592653589793 float_absolute false boolean_positive exec_if 3.141592653589793 float_cbrt float_- false float_= true float_* close exec_if)
;;; Best program: (float_cbrt float_+ boolean_and true exec_dup (exec_dup (false float_+ boolean_= exec_dup (false float_absolute float_+ 3.141592653589793 float_absolute false boolean_positive exec_if (3.141592653589793 float_cbrt float_- false float_= true float_*) (exec_if () ())))))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 21
;;; -------------------------------------------------------
;;; Best plushy: (exec_if true float_* boolean_= float_cbrt float_= boolean_positive exec_if float_= boolean_or exec_dup float_- float_absolute float_cbrt exec_dup close float_cbrt boolean_and 2.718281828459045 float_sqrt 3.141592653589793 boolean_or exec_if boolean_positive in1 boolean_and boolean_positive float_+ exec_if boolean_not in1 exec_if)
;;; Best program: (exec_if (true float_* boolean_= float_cbrt float_= boolean_positive exec_if (float_= boolean_or exec_dup (float_- float_absolute float_cbrt exec_dup () float_cbrt boolean_and 2.718281828459045 float_sqrt 3.141592653589793 boolean_or exec_if (boolean_positive in1 boolean_and boolean_positive float_+ exec_if (boolean_not in1 exec_if () ()) ()) ())) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 22
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or float_absolute exec_if float_sqrt float_= float_cbrt float_- close boolean_or float_= false exec_dup float_cbrt exec_dup boolean_positive true boolean_positive boolean_not 3.141592653589793 float_absolute exec_dup float_absolute float_absolute true)
;;; Best program: (boolean_or float_absolute exec_if (float_sqrt float_= float_cbrt float_-) (boolean_or float_= false exec_dup (float_cbrt exec_dup (boolean_positive true boolean_positive boolean_not 3.141592653589793 float_absolute exec_dup (float_absolute float_absolute true)))))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 23
;;; -------------------------------------------------------
;;; Best plushy: (float_cbrt exec_if exec_if boolean_and boolean_positive float_= float_cbrt false boolean_or false boolean_= boolean_or float_absolute boolean_or in1 float_+ false float_- float_= boolean_or float_+ false 3.141592653589793 boolean_and boolean_= in1 boolean_= boolean_not float_+)
;;; Best program: (float_cbrt exec_if (exec_if (boolean_and boolean_positive float_= float_cbrt false boolean_or false boolean_= boolean_or float_absolute boolean_or in1 float_+ false float_- float_= boolean_or float_+ false 3.141592653589793 boolean_and boolean_= in1 boolean_= boolean_not float_+) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 24
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt float_+ float_+ float_sqrt float_= boolean_negative float_+ in1 float_cbrt exec_dup float_- exec_dup float_absolute 3.141592653589793 exec_dup in1 false exec_dup boolean_or boolean_and float_- boolean_positive boolean_positive)
;;; Best program: (float_sqrt float_+ float_+ float_sqrt float_= boolean_negative float_+ in1 float_cbrt exec_dup (float_- exec_dup (float_absolute 3.141592653589793 exec_dup (in1 false exec_dup (boolean_or boolean_and float_- boolean_positive boolean_positive)))))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 25
;;; -------------------------------------------------------
;;; Best plushy: (2.718281828459045 true exec_if boolean_and boolean_positive float_= float_cbrt float_= false exec_if boolean_= float_= float_absolute float_= close float_= boolean_positive float_+ 3.141592653589793 true in1 float_absolute boolean_= float_absolute in1 float_+)
;;; Best program: (2.718281828459045 true exec_if (boolean_and boolean_positive float_= float_cbrt float_= false exec_if (boolean_= float_= float_absolute float_=) (float_= boolean_positive float_+ 3.141592653589793 true in1 float_absolute boolean_= float_absolute in1 float_+)) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 26
;;; -------------------------------------------------------
;;; Best plushy: (exec_if float_+ boolean_or boolean_negative 3.141592653589793 float_= float_= close float_sqrt exec_if boolean_= 2.718281828459045 float_absolute boolean_positive 3.141592653589793 float_+ boolean_= in1 3.141592653589793 3.141592653589793 boolean_and exec_dup)
;;; Best program: (exec_if (float_+ boolean_or boolean_negative 3.141592653589793 float_= float_=) (float_sqrt exec_if (boolean_= 2.718281828459045 float_absolute boolean_positive 3.141592653589793 float_+ boolean_= in1 3.141592653589793 3.141592653589793 boolean_and exec_dup ()) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 27
;;; -------------------------------------------------------
;;; Best plushy: (in1 true float_sqrt float_- exec_dup float_= boolean_or float_absolute boolean_or false close boolean_= float_absolute boolean_and close boolean_= 3.141592653589793 float_absolute in1 float_- 2.718281828459045 in1 exec_if boolean_not float_-)
;;; Best program: (in1 true float_sqrt float_- exec_dup (float_= boolean_or float_absolute boolean_or false) boolean_= float_absolute boolean_and boolean_= 3.141592653589793 float_absolute in1 float_- 2.718281828459045 in1 exec_if (boolean_not float_-) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 28
;;; -------------------------------------------------------
;;; Best plushy: (exec_if boolean_not false float_- boolean_negative false false float_= boolean_not float_+ float_= close float_sqrt exec_dup float_- float_+ float_= float_cbrt exec_dup close float_cbrt float_+ boolean_and 3.141592653589793 float_sqrt exec_dup boolean_or boolean_positive 3.141592653589793 float_+ boolean_not)
;;; Best program: (exec_if (boolean_not false float_- boolean_negative false false float_= boolean_not float_+ float_=) (float_sqrt exec_dup (float_- float_+ float_= float_cbrt exec_dup () float_cbrt float_+ boolean_and 3.141592653589793 float_sqrt exec_dup (boolean_or boolean_positive 3.141592653589793 float_+ boolean_not))))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 29
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute boolean_not boolean_= float_= boolean_positive true boolean_or exec_if float_cbrt float_- close true 2.718281828459045 float_absolute close float_sqrt float_sqrt true 3.141592653589793 float_+ float_+ boolean_not boolean_= close)
;;; Best program: (float_absolute boolean_not boolean_= float_= boolean_positive true boolean_or exec_if (float_cbrt float_-) (true 2.718281828459045 float_absolute) float_sqrt float_sqrt true 3.141592653589793 float_+ float_+ boolean_not boolean_=)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 30
;;; -------------------------------------------------------
;;; Best plushy: (in1 2.718281828459045 float_+ boolean_positive true float_sqrt float_= float_sqrt float_absolute boolean_not float_sqrt float_absolute boolean_positive float_= in1 true exec_dup boolean_not float_+ 3.141592653589793 float_- exec_if boolean_negative boolean_or boolean_and boolean_or)
;;; Best program: (in1 2.718281828459045 float_+ boolean_positive true float_sqrt float_= float_sqrt float_absolute boolean_not float_sqrt float_absolute boolean_positive float_= in1 true exec_dup (boolean_not float_+ 3.141592653589793 float_- exec_if (boolean_negative boolean_or boolean_and boolean_or) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 31
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or float_+ boolean_negative boolean_negative true float_+ boolean_positive float_cbrt false in1 boolean_= exec_dup close float_absolute 3.141592653589793 exec_if float_sqrt close false boolean_negative boolean_and float_- boolean_and boolean_not boolean_= float_absolute)
;;; Best program: (boolean_or float_+ boolean_negative boolean_negative true float_+ boolean_positive float_cbrt false in1 boolean_= exec_dup () float_absolute 3.141592653589793 exec_if (float_sqrt) (false boolean_negative boolean_and float_- boolean_and boolean_not boolean_= float_absolute))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 32
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt float_- float_absolute float_cbrt float_cbrt float_= float_+ boolean_and float_* boolean_and float_- false float_sqrt float_absolute float_absolute exec_dup true 3.141592653589793 boolean_not float_absolute float_absolute 2.718281828459045 float_+ float_= exec_if boolean_negative boolean_not 3.141592653589793)
;;; Best program: (float_sqrt float_- float_absolute float_cbrt float_cbrt float_= float_+ boolean_and float_* boolean_and float_- false float_sqrt float_absolute float_absolute exec_dup (true 3.141592653589793 boolean_not float_absolute float_absolute 2.718281828459045 float_+ float_= exec_if (boolean_negative boolean_not 3.141592653589793) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 33
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute close exec_if boolean_= boolean_= boolean_positive boolean_or float_= boolean_positive exec_dup exec_if float_* 3.141592653589793 float_cbrt boolean_not 3.141592653589793 float_- 2.718281828459045 3.141592653589793 true float_cbrt 2.718281828459045)
;;; Best program: (float_absolute exec_if (boolean_= boolean_= boolean_positive boolean_or float_= boolean_positive exec_dup (exec_if (float_* 3.141592653589793 float_cbrt boolean_not 3.141592653589793 float_- 2.718281828459045 3.141592653589793 true float_cbrt 2.718281828459045) ())) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 34
;;; -------------------------------------------------------
;;; Best plushy: (boolean_positive exec_if boolean_= float_= float_cbrt exec_if boolean_not false float_- float_absolute boolean_positive float_+ boolean_negative true 3.141592653589793 float_= boolean_= float_+ close float_absolute boolean_negative)
;;; Best program: (boolean_positive exec_if (boolean_= float_= float_cbrt exec_if (boolean_not false float_- float_absolute boolean_positive float_+ boolean_negative true 3.141592653589793 float_= boolean_= float_+) (float_absolute boolean_negative)) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 35
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_+ float_* float_+ float_cbrt exec_dup boolean_negative boolean_or float_absolute float_* in1 boolean_negative boolean_positive boolean_not close in1 false float_absolute exec_if close 3.141592653589793 close exec_if in1 2.718281828459045)
;;; Best program: (in1 float_+ float_* float_+ float_cbrt exec_dup (boolean_negative boolean_or float_absolute float_* in1 boolean_negative boolean_positive boolean_not) in1 false float_absolute exec_if () (3.141592653589793) exec_if (in1 2.718281828459045) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 36
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute float_sqrt float_* boolean_= float_cbrt boolean_= boolean_or close boolean_or float_= boolean_or boolean_not boolean_and boolean_not in1 float_sqrt exec_if boolean_positive float_cbrt float_absolute 2.718281828459045 float_* boolean_positive close boolean_= exec_dup boolean_or exec_dup float_cbrt 3.141592653589793)
;;; Best program: (float_absolute float_sqrt float_* boolean_= float_cbrt boolean_= boolean_or boolean_or float_= boolean_or boolean_not boolean_and boolean_not in1 float_sqrt exec_if (boolean_positive float_cbrt float_absolute 2.718281828459045 float_* boolean_positive) (boolean_= exec_dup (boolean_or exec_dup (float_cbrt 3.141592653589793))))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 37
;;; -------------------------------------------------------
;;; Best plushy: (exec_if 2.718281828459045 boolean_positive boolean_= float_cbrt boolean_positive 2.718281828459045 boolean_and float_+ exec_dup false false boolean_= float_cbrt boolean_and float_absolute in1 boolean_and exec_dup float_= float_absolute float_= 3.141592653589793 boolean_positive float_+ boolean_and 3.141592653589793 true)
;;; Best program: (exec_if (2.718281828459045 boolean_positive boolean_= float_cbrt boolean_positive 2.718281828459045 boolean_and float_+ exec_dup (false false boolean_= float_cbrt boolean_and float_absolute in1 boolean_and exec_dup (float_= float_absolute float_= 3.141592653589793 boolean_positive float_+ boolean_and 3.141592653589793 true))) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 38
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt boolean_or float_sqrt float_= float_sqrt float_= float_absolute boolean_and exec_if close 2.718281828459045 float_+ boolean_positive exec_if float_+ 3.141592653589793 2.718281828459045 in1 boolean_= false close boolean_and float_absolute in1 close boolean_positive exec_if)
;;; Best program: (float_sqrt boolean_or float_sqrt float_= float_sqrt float_= float_absolute boolean_and exec_if () (2.718281828459045 float_+ boolean_positive exec_if (float_+ 3.141592653589793 2.718281828459045 in1 boolean_= false) (boolean_and float_absolute in1) boolean_positive exec_if () ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 39
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute exec_if boolean_= boolean_negative boolean_negative float_* float_sqrt float_= float_+ boolean_or float_absolute float_absolute float_absolute false float_= in1 boolean_positive boolean_and exec_if float_sqrt false true boolean_= float_= false float_cbrt boolean_or boolean_= 3.141592653589793 float_absolute)
;;; Best program: (float_absolute exec_if (boolean_= boolean_negative boolean_negative float_* float_sqrt float_= float_+ boolean_or float_absolute float_absolute float_absolute false float_= in1 boolean_positive boolean_and exec_if (float_sqrt false true boolean_= float_= false float_cbrt boolean_or boolean_= 3.141592653589793 float_absolute) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 40
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= boolean_not boolean_= boolean_negative boolean_= float_= boolean_or boolean_or float_+ float_absolute float_= float_= false exec_if float_= boolean_and true float_absolute float_sqrt boolean_not in1 float_= boolean_= boolean_negative 2.718281828459045 boolean_and float_absolute)
;;; Best program: (boolean_= boolean_not boolean_= boolean_negative boolean_= float_= boolean_or boolean_or float_+ float_absolute float_= float_= false exec_if (float_= boolean_and true float_absolute float_sqrt boolean_not in1 float_= boolean_= boolean_negative 2.718281828459045 boolean_and float_absolute) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 41
;;; -------------------------------------------------------
;;; Best plushy: (exec_if exec_if boolean_and boolean_negative float_cbrt float_sqrt float_= float_= float_= float_cbrt float_= float_absolute exec_dup float_- true exec_if false float_= 3.141592653589793 float_- exec_if)
;;; Best program: (exec_if (exec_if (boolean_and boolean_negative float_cbrt float_sqrt float_= float_= float_= float_cbrt float_= float_absolute exec_dup (float_- true exec_if (false float_= 3.141592653589793 float_- exec_if () ()) ())) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 42
;;; -------------------------------------------------------
;;; Best plushy: (in1 true float_cbrt true float_cbrt float_= boolean_or exec_if false exec_dup float_- 3.141592653589793 false boolean_positive boolean_and in1 3.141592653589793 float_+ boolean_positive 3.141592653589793 float_absolute boolean_= close 3.141592653589793 float_cbrt 2.718281828459045)
;;; Best program: (in1 true float_cbrt true float_cbrt float_= boolean_or exec_if (false exec_dup (float_- 3.141592653589793 false boolean_positive boolean_and in1 3.141592653589793 float_+ boolean_positive 3.141592653589793 float_absolute boolean_=) 3.141592653589793 float_cbrt 2.718281828459045) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 43
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt boolean_or float_= boolean_positive false exec_dup false exec_dup float_sqrt exec_if exec_if float_= in1 3.141592653589793 3.141592653589793 2.718281828459045 close boolean_not boolean_not close in1 in1 3.141592653589793)
;;; Best program: (float_sqrt boolean_or float_= boolean_positive false exec_dup (false exec_dup (float_sqrt exec_if (exec_if (float_= in1 3.141592653589793 3.141592653589793 2.718281828459045) (boolean_not boolean_not) in1 in1 3.141592653589793) ())))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 44
;;; -------------------------------------------------------
;;; Best plushy: (float_cbrt exec_if boolean_= float_* float_cbrt float_cbrt false float_cbrt false exec_if false float_= float_= boolean_positive float_sqrt float_sqrt 2.718281828459045 boolean_negative boolean_positive boolean_and exec_if boolean_not 3.141592653589793 boolean_and)
;;; Best program: (float_cbrt exec_if (boolean_= float_* float_cbrt float_cbrt false float_cbrt false exec_if (false float_= float_= boolean_positive float_sqrt float_sqrt 2.718281828459045 boolean_negative boolean_positive boolean_and exec_if (boolean_not 3.141592653589793 boolean_and) ()) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 45
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt float_cbrt float_cbrt boolean_negative boolean_negative float_= boolean_or float_= false float_cbrt float_sqrt exec_if float_cbrt close exec_if exec_if boolean_= float_cbrt 2.718281828459045 exec_if exec_dup boolean_= exec_if boolean_and close boolean_negative)
;;; Best program: (float_sqrt float_cbrt float_cbrt boolean_negative boolean_negative float_= boolean_or float_= false float_cbrt float_sqrt exec_if (float_cbrt) (exec_if (exec_if (boolean_= float_cbrt 2.718281828459045 exec_if (exec_dup (boolean_= exec_if (boolean_and) (boolean_negative))) ()) ()) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 46
;;; -------------------------------------------------------
;;; Best plushy: (float_cbrt boolean_or true boolean_= float_cbrt float_= 3.141592653589793 false boolean_positive false float_sqrt float_= exec_dup float_sqrt exec_if false float_= false 2.718281828459045 boolean_or 2.718281828459045 3.141592653589793 boolean_not float_absolute in1)
;;; Best program: (float_cbrt boolean_or true boolean_= float_cbrt float_= 3.141592653589793 false boolean_positive false float_sqrt float_= exec_dup (float_sqrt exec_if (false float_= false 2.718281828459045 boolean_or 2.718281828459045 3.141592653589793 boolean_not float_absolute in1) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 47
;;; -------------------------------------------------------
;;; Best plushy: (float_cbrt true boolean_= boolean_or float_absolute true float_sqrt boolean_positive exec_if float_sqrt float_absolute 2.718281828459045 float_+ boolean_not false boolean_not float_+ close float_absolute float_absolute false boolean_negative boolean_or 3.141592653589793 boolean_not float_* exec_if close)
;;; Best program: (float_cbrt true boolean_= boolean_or float_absolute true float_sqrt boolean_positive exec_if (float_sqrt float_absolute 2.718281828459045 float_+ boolean_not false boolean_not float_+) (float_absolute float_absolute false boolean_negative boolean_or 3.141592653589793 boolean_not float_* exec_if () ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 48
;;; -------------------------------------------------------
;;; Best plushy: (in1 boolean_negative boolean_and boolean_or boolean_positive float_= 2.718281828459045 boolean_or boolean_negative 2.718281828459045 exec_if float_= false float_absolute in1 close false boolean_not false true float_= true float_absolute float_- boolean_negative float_cbrt boolean_negative float_absolute)
;;; Best program: (in1 boolean_negative boolean_and boolean_or boolean_positive float_= 2.718281828459045 boolean_or boolean_negative 2.718281828459045 exec_if (float_= false float_absolute in1) (false boolean_not false true float_= true float_absolute float_- boolean_negative float_cbrt boolean_negative float_absolute))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 49
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_cbrt boolean_or float_* boolean_or boolean_and float_= exec_if float_sqrt float_sqrt false float_absolute exec_if float_- float_cbrt false boolean_not boolean_and boolean_not 2.718281828459045 float_- boolean_= boolean_or float_absolute in1 float_* float_absolute boolean_or boolean_=)
;;; Best program: (in1 float_cbrt boolean_or float_* boolean_or boolean_and float_= exec_if (float_sqrt float_sqrt false float_absolute exec_if (float_- float_cbrt false boolean_not boolean_and boolean_not 2.718281828459045 float_- boolean_= boolean_or float_absolute in1 float_* float_absolute boolean_or boolean_=) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 50
;;; -------------------------------------------------------
;;; Best plushy: (false float_- float_cbrt float_cbrt boolean_or close float_sqrt exec_if float_- float_- boolean_positive boolean_and boolean_= float_- exec_if float_= boolean_or 2.718281828459045 3.141592653589793 float_= exec_if float_+ close)
;;; Best program: (false float_- float_cbrt float_cbrt boolean_or float_sqrt exec_if (float_- float_- boolean_positive boolean_and boolean_= float_- exec_if (float_= boolean_or 2.718281828459045 3.141592653589793 float_= exec_if (float_+) ()) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 51
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt float_cbrt boolean_= exec_if boolean_or float_= boolean_= false float_= false float_absolute boolean_positive float_absolute close in1 boolean_positive boolean_and exec_if boolean_positive boolean_= exec_if true boolean_and true in1 float_+ boolean_and)
;;; Best program: (float_sqrt float_cbrt boolean_= exec_if (boolean_or float_= boolean_= false float_= false float_absolute boolean_positive float_absolute) (in1 boolean_positive boolean_and exec_if (boolean_positive boolean_= exec_if (true boolean_and true in1 float_+ boolean_and) ()) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 52
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_cbrt boolean_= float_cbrt float_* boolean_and float_sqrt boolean_negative float_- boolean_negative 3.141592653589793 float_- boolean_not exec_if float_absolute boolean_positive 2.718281828459045 boolean_and 2.718281828459045 boolean_positive float_cbrt boolean_= false float_cbrt in1 exec_if float_absolute)
;;; Best program: (in1 float_cbrt boolean_= float_cbrt float_* boolean_and float_sqrt boolean_negative float_- boolean_negative 3.141592653589793 float_- boolean_not exec_if (float_absolute boolean_positive 2.718281828459045 boolean_and 2.718281828459045 boolean_positive float_cbrt boolean_= false float_cbrt in1 exec_if (float_absolute) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 53
;;; -------------------------------------------------------
;;; Best plushy: (in1 true float_absolute exec_if float_cbrt boolean_or close boolean_negative float_= exec_if false float_absolute float_absolute false boolean_positive float_- float_sqrt float_= boolean_positive boolean_= exec_if float_absolute 2.718281828459045 boolean_and true 3.141592653589793 boolean_not float_+ boolean_and)
;;; Best program: (in1 true float_absolute exec_if (float_cbrt boolean_or) (boolean_negative float_= exec_if (false float_absolute float_absolute false boolean_positive float_- float_sqrt float_= boolean_positive boolean_= exec_if (float_absolute 2.718281828459045 boolean_and true 3.141592653589793 boolean_not float_+ boolean_and) ()) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 54
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt boolean_= float_cbrt float_sqrt boolean_= float_cbrt float_sqrt float_sqrt boolean_positive float_* false exec_if float_sqrt boolean_positive exec_if boolean_positive boolean_or 3.141592653589793 float_+ float_absolute close 2.718281828459045 2.718281828459045 float_absolute boolean_not in1 close)
;;; Best program: (float_sqrt boolean_= float_cbrt float_sqrt boolean_= float_cbrt float_sqrt float_sqrt boolean_positive float_* false exec_if (float_sqrt boolean_positive exec_if (boolean_positive boolean_or 3.141592653589793 float_+ float_absolute) (2.718281828459045 2.718281828459045 float_absolute boolean_not in1)) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 55
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or true float_absolute boolean_or float_= exec_dup boolean_not float_* false float_sqrt float_= boolean_= float_cbrt close in1 exec_if boolean_not boolean_or exec_if float_sqrt float_- false float_absolute boolean_and boolean_= float_= boolean_or false false close 2.718281828459045 in1 exec_if close)
;;; Best program: (boolean_or true float_absolute boolean_or float_= exec_dup (boolean_not float_* false float_sqrt float_= boolean_= float_cbrt) in1 exec_if (boolean_not boolean_or exec_if (float_sqrt float_- false float_absolute boolean_and boolean_= float_= boolean_or false false) (2.718281828459045 in1 exec_if () ())) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 56
;;; -------------------------------------------------------
;;; Best plushy: (float_- boolean_= float_cbrt boolean_= boolean_or close boolean_and boolean_or exec_if float_+ float_cbrt float_- close float_= boolean_positive boolean_and float_sqrt exec_if float_absolute float_= in1 3.141592653589793 close exec_if false boolean_and boolean_positive float_+)
;;; Best program: (float_- boolean_= float_cbrt boolean_= boolean_or boolean_and boolean_or exec_if (float_+ float_cbrt float_-) (float_= boolean_positive boolean_and float_sqrt exec_if (float_absolute float_= in1 3.141592653589793) (exec_if (false boolean_and boolean_positive float_+) ())))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 57
;;; -------------------------------------------------------
;;; Best plushy: (in1 boolean_= boolean_= boolean_or boolean_positive boolean_= float_sqrt float_cbrt boolean_positive false float_= boolean_= float_- float_sqrt float_absolute boolean_not 3.141592653589793 boolean_positive boolean_positive boolean_positive float_absolute 3.141592653589793 exec_if boolean_not in1 boolean_positive boolean_or)
;;; Best program: (in1 boolean_= boolean_= boolean_or boolean_positive boolean_= float_sqrt float_cbrt boolean_positive false float_= boolean_= float_- float_sqrt float_absolute boolean_not 3.141592653589793 boolean_positive boolean_positive boolean_positive float_absolute 3.141592653589793 exec_if (boolean_not in1 boolean_positive boolean_or) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 58
;;; -------------------------------------------------------
;;; Best plushy: (boolean_negative boolean_negative exec_if float_cbrt float_* boolean_or in1 float_sqrt boolean_or false close float_cbrt float_sqrt boolean_= boolean_not exec_dup float_absolute float_* float_= true 2.718281828459045 boolean_and boolean_not float_- exec_dup)
;;; Best program: (boolean_negative boolean_negative exec_if (float_cbrt float_* boolean_or in1 float_sqrt boolean_or false) (float_cbrt float_sqrt boolean_= boolean_not exec_dup (float_absolute float_* float_= true 2.718281828459045 boolean_and boolean_not float_- exec_dup ())))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 59
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute float_- boolean_= boolean_not exec_if exec_if float_cbrt boolean_= float_sqrt float_sqrt float_sqrt boolean_= float_- float_cbrt boolean_or float_absolute boolean_not boolean_and float_= float_= float_= boolean_and true float_+ boolean_negative float_absolute boolean_or boolean_or true exec_dup exec_dup)
;;; Best program: (float_absolute float_- boolean_= boolean_not exec_if (exec_if (float_cbrt boolean_= float_sqrt float_sqrt float_sqrt boolean_= float_- float_cbrt boolean_or float_absolute boolean_not boolean_and float_= float_= float_= boolean_and true float_+ boolean_negative float_absolute boolean_or boolean_or true exec_dup (exec_dup ())) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 60
;;; -------------------------------------------------------
;;; Best plushy: (float_- boolean_not float_- boolean_or float_- boolean_negative true exec_if boolean_or boolean_negative boolean_or 2.718281828459045 float_sqrt float_sqrt false float_cbrt false float_sqrt 2.718281828459045 boolean_positive float_sqrt 2.718281828459045 2.718281828459045 3.141592653589793 3.141592653589793 close in1 2.718281828459045 in1 close)
;;; Best program: (float_- boolean_not float_- boolean_or float_- boolean_negative true exec_if (boolean_or boolean_negative boolean_or 2.718281828459045 float_sqrt float_sqrt false float_cbrt false float_sqrt 2.718281828459045 boolean_positive float_sqrt 2.718281828459045 2.718281828459045 3.141592653589793 3.141592653589793) (in1 2.718281828459045 in1))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 61
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt boolean_positive 2.718281828459045 boolean_negative float_+ float_sqrt boolean_or exec_dup float_cbrt float_= float_= float_sqrt boolean_positive float_cbrt boolean_or boolean_negative 3.141592653589793 false exec_if 2.718281828459045 exec_dup float_= float_absolute float_= boolean_or 2.718281828459045 boolean_negative)
;;; Best program: (float_sqrt boolean_positive 2.718281828459045 boolean_negative float_+ float_sqrt boolean_or exec_dup (float_cbrt float_= float_= float_sqrt boolean_positive float_cbrt boolean_or boolean_negative 3.141592653589793 false exec_if (2.718281828459045 exec_dup (float_= float_absolute float_= boolean_or 2.718281828459045 boolean_negative)) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 62
;;; -------------------------------------------------------
;;; Best plushy: (2.718281828459045 float_- float_cbrt boolean_or float_+ boolean_negative boolean_and false boolean_= float_sqrt float_- boolean_or boolean_positive float_sqrt close float_= float_= true close true true false in1 boolean_and boolean_= float_cbrt false boolean_not 3.141592653589793 exec_if boolean_or true boolean_not boolean_negative in1 float_absolute)
;;; Best program: (2.718281828459045 float_- float_cbrt boolean_or float_+ boolean_negative boolean_and false boolean_= float_sqrt float_- boolean_or boolean_positive float_sqrt float_= float_= true true true false in1 boolean_and boolean_= float_cbrt false boolean_not 3.141592653589793 exec_if (boolean_or true boolean_not boolean_negative in1 float_absolute) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 63
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or float_absolute boolean_= 3.141592653589793 float_cbrt 3.141592653589793 close boolean_or false float_absolute float_- float_absolute float_= float_- float_absolute float_= in1 boolean_or exec_if boolean_and exec_if float_cbrt float_cbrt boolean_and float_cbrt)
;;; Best program: (boolean_or float_absolute boolean_= 3.141592653589793 float_cbrt 3.141592653589793 boolean_or false float_absolute float_- float_absolute float_= float_- float_absolute float_= in1 boolean_or exec_if (boolean_and exec_if (float_cbrt float_cbrt boolean_and float_cbrt) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 64
;;; -------------------------------------------------------
;;; Best plushy: (float_sqrt boolean_positive boolean_= float_cbrt float_+ float_- float_sqrt boolean_or float_- float_* float_= float_= float_= boolean_not float_cbrt boolean_= float_cbrt 3.141592653589793 boolean_negative 2.718281828459045 boolean_positive float_absolute false true boolean_not boolean_negative float_- boolean_not boolean_= float_= 2.718281828459045 3.141592653589793 exec_if)
;;; Best program: (float_sqrt boolean_positive boolean_= float_cbrt float_+ float_- float_sqrt boolean_or float_- float_* float_= float_= float_= boolean_not float_cbrt boolean_= float_cbrt 3.141592653589793 boolean_negative 2.718281828459045 boolean_positive float_absolute false true boolean_not boolean_negative float_- boolean_not boolean_= float_= 2.718281828459045 3.141592653589793 exec_if () ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 65
;;; -------------------------------------------------------
;;; Best plushy: (float_- float_- float_cbrt float_cbrt boolean_or boolean_or false 2.718281828459045 float_cbrt boolean_= float_cbrt boolean_or float_= boolean_not float_* float_= close 2.718281828459045 float_= boolean_or float_sqrt float_+ 2.718281828459045 2.718281828459045 exec_if float_sqrt boolean_negative boolean_not in1)
;;; Best program: (float_- float_- float_cbrt float_cbrt boolean_or boolean_or false 2.718281828459045 float_cbrt boolean_= float_cbrt boolean_or float_= boolean_not float_* float_= 2.718281828459045 float_= boolean_or float_sqrt float_+ 2.718281828459045 2.718281828459045 exec_if (float_sqrt boolean_negative boolean_not in1) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 66
;;; -------------------------------------------------------
;;; Best plushy: (in1 float_* float_cbrt float_cbrt float_absolute boolean_and boolean_or boolean_or 2.718281828459045 boolean_or float_- float_cbrt boolean_positive boolean_not boolean_not 3.141592653589793 boolean_positive true boolean_negative true float_* 3.141592653589793 2.718281828459045 boolean_negative 2.718281828459045 close close boolean_= float_absolute)
;;; Best program: (in1 float_* float_cbrt float_cbrt float_absolute boolean_and boolean_or boolean_or 2.718281828459045 boolean_or float_- float_cbrt boolean_positive boolean_not boolean_not 3.141592653589793 boolean_positive true boolean_negative true float_* 3.141592653589793 2.718281828459045 boolean_negative 2.718281828459045 boolean_= float_absolute)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 67
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute false boolean_= float_absolute float_- boolean_positive float_cbrt boolean_or float_sqrt boolean_not 2.718281828459045 2.718281828459045 float_cbrt false in1 float_absolute false float_absolute float_= boolean_positive boolean_and 2.718281828459045 2.718281828459045 float_sqrt float_absolute exec_dup boolean_not boolean_positive boolean_=)
;;; Best program: (float_absolute false boolean_= float_absolute float_- boolean_positive float_cbrt boolean_or float_sqrt boolean_not 2.718281828459045 2.718281828459045 float_cbrt false in1 float_absolute false float_absolute float_= boolean_positive boolean_and 2.718281828459045 2.718281828459045 float_sqrt float_absolute exec_dup (boolean_not boolean_positive boolean_=))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 68
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or boolean_or boolean_= float_- float_absolute close float_sqrt float_sqrt boolean_not true boolean_positive false in1 boolean_positive boolean_positive boolean_positive float_sqrt boolean_= float_- boolean_= boolean_not float_sqrt boolean_= boolean_and true false)
;;; Best program: (boolean_or boolean_or boolean_= float_- float_absolute float_sqrt float_sqrt boolean_not true boolean_positive false in1 boolean_positive boolean_positive boolean_positive float_sqrt boolean_= float_- boolean_= boolean_not float_sqrt boolean_= boolean_and true false)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 69
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or float_cbrt float_absolute false float_cbrt 2.718281828459045 boolean_or boolean_and float_= boolean_and boolean_not exec_dup close true float_= float_= true float_sqrt 3.141592653589793 false float_= 2.718281828459045 boolean_positive in1 float_absolute boolean_negative)
;;; Best program: (boolean_or float_cbrt float_absolute false float_cbrt 2.718281828459045 boolean_or boolean_and float_= boolean_and boolean_not exec_dup () true float_= float_= true float_sqrt 3.141592653589793 false float_= 2.718281828459045 boolean_positive in1 float_absolute boolean_negative)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 70
;;; -------------------------------------------------------
;;; Best plushy: (exec_dup float_- float_- boolean_or boolean_= float_absolute boolean_or float_sqrt boolean_positive true float_absolute boolean_= float_- float_absolute false boolean_and boolean_= 2.718281828459045 3.141592653589793 boolean_positive float_cbrt 2.718281828459045 boolean_= boolean_or float_- 2.718281828459045 float_sqrt)
;;; Best program: (exec_dup (float_- float_- boolean_or boolean_= float_absolute boolean_or float_sqrt boolean_positive true float_absolute boolean_= float_- float_absolute false boolean_and boolean_= 2.718281828459045 3.141592653589793 boolean_positive float_cbrt 2.718281828459045 boolean_= boolean_or float_- 2.718281828459045 float_sqrt))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 71
;;; -------------------------------------------------------
;;; Best plushy: (2.718281828459045 float_cbrt boolean_= float_- exec_if float_sqrt float_- 2.718281828459045 boolean_negative boolean_positive float_sqrt boolean_= boolean_and float_cbrt true close boolean_not close float_absolute boolean_positive boolean_not float_absolute float_= 3.141592653589793 boolean_not float_- boolean_positive boolean_=)
;;; Best program: (2.718281828459045 float_cbrt boolean_= float_- exec_if (float_sqrt float_- 2.718281828459045 boolean_negative boolean_positive float_sqrt boolean_= boolean_and float_cbrt true) (boolean_not) float_absolute boolean_positive boolean_not float_absolute float_= 3.141592653589793 boolean_not float_- boolean_positive boolean_=)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 72
;;; -------------------------------------------------------
;;; Best plushy: (float_* boolean_= boolean_or 3.141592653589793 exec_if boolean_or float_sqrt boolean_or boolean_and float_* boolean_positive boolean_positive boolean_positive close exec_if 3.141592653589793 3.141592653589793 true 3.141592653589793 float_absolute true 3.141592653589793 float_absolute false boolean_negative 2.718281828459045 float_absolute)
;;; Best program: (float_* boolean_= boolean_or 3.141592653589793 exec_if (boolean_or float_sqrt boolean_or boolean_and float_* boolean_positive boolean_positive boolean_positive) (exec_if (3.141592653589793 3.141592653589793 true 3.141592653589793 float_absolute true 3.141592653589793 float_absolute false boolean_negative 2.718281828459045 float_absolute) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 73
;;; -------------------------------------------------------
;;; Best plushy: (exec_dup float_cbrt float_- boolean_= float_- float_cbrt boolean_= boolean_and float_cbrt float_absolute 2.718281828459045 boolean_positive boolean_and float_cbrt exec_if float_sqrt float_sqrt true float_cbrt true false exec_if float_- boolean_negative float_+ float_absolute close float_absolute)
;;; Best program: (exec_dup (float_cbrt float_- boolean_= float_- float_cbrt boolean_= boolean_and float_cbrt float_absolute 2.718281828459045 boolean_positive boolean_and float_cbrt exec_if (float_sqrt float_sqrt true float_cbrt true false exec_if (float_- boolean_negative float_+ float_absolute) (float_absolute)) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 74
;;; -------------------------------------------------------
;;; Best plushy: (float_- boolean_= false boolean_or float_cbrt float_- 2.718281828459045 boolean_or float_= float_absolute close false float_* boolean_negative exec_dup close boolean_negative boolean_negative float_= boolean_positive boolean_or boolean_positive boolean_not float_* true boolean_negative float_cbrt 2.718281828459045 in1 true close in1)
;;; Best program: (float_- boolean_= false boolean_or float_cbrt float_- 2.718281828459045 boolean_or float_= float_absolute false float_* boolean_negative exec_dup () boolean_negative boolean_negative float_= boolean_positive boolean_or boolean_positive boolean_not float_* true boolean_negative float_cbrt 2.718281828459045 in1 true in1)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 75
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or boolean_not float_cbrt close boolean_not boolean_and float_- float_cbrt float_sqrt float_* boolean_or close float_* float_absolute float_sqrt exec_dup true false float_= in1 2.718281828459045 float_= exec_if boolean_and exec_if float_+ boolean_= close float_sqrt 3.141592653589793 boolean_negative float_cbrt 2.718281828459045 in1 close)
;;; Best program: (boolean_or boolean_not float_cbrt boolean_not boolean_and float_- float_cbrt float_sqrt float_* boolean_or float_* float_absolute float_sqrt exec_dup (true false float_= in1 2.718281828459045 float_= exec_if (boolean_and exec_if (float_+ boolean_=) (float_sqrt 3.141592653589793 boolean_negative float_cbrt 2.718281828459045 in1)) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 76
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute boolean_positive float_- 3.141592653589793 false exec_dup close float_cbrt 2.718281828459045 float_cbrt float_+ float_absolute boolean_positive float_absolute float_- false float_sqrt boolean_negative 3.141592653589793 boolean_or boolean_positive 2.718281828459045 boolean_= boolean_not in1 2.718281828459045 2.718281828459045 exec_if)
;;; Best program: (float_absolute boolean_positive float_- 3.141592653589793 false exec_dup () float_cbrt 2.718281828459045 float_cbrt float_+ float_absolute boolean_positive float_absolute float_- false float_sqrt boolean_negative 3.141592653589793 boolean_or boolean_positive 2.718281828459045 boolean_= boolean_not in1 2.718281828459045 2.718281828459045 exec_if () ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 77
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute boolean_positive float_- 3.141592653589793 false exec_dup close float_cbrt 2.718281828459045 float_cbrt float_+ float_absolute boolean_positive float_absolute float_- false float_sqrt boolean_negative 3.141592653589793 boolean_or boolean_positive 2.718281828459045 boolean_= boolean_not in1 2.718281828459045 2.718281828459045 exec_if)
;;; Best program: (float_absolute boolean_positive float_- 3.141592653589793 false exec_dup () float_cbrt 2.718281828459045 float_cbrt float_+ float_absolute boolean_positive float_absolute float_- false float_sqrt boolean_negative 3.141592653589793 boolean_or boolean_positive 2.718281828459045 boolean_= boolean_not in1 2.718281828459045 2.718281828459045 exec_if () ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 78
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute boolean_= boolean_= float_- boolean_or boolean_= exec_dup float_absolute float_- boolean_positive float_- float_= true boolean_and exec_dup boolean_and true close exec_if 2.718281828459045 3.141592653589793 exec_dup float_sqrt 2.718281828459045 float_absolute float_cbrt exec_if exec_if)
;;; Best program: (float_absolute boolean_= boolean_= float_- boolean_or boolean_= exec_dup (float_absolute float_- boolean_positive float_- float_= true boolean_and exec_dup (boolean_and true) exec_if (2.718281828459045 3.141592653589793 exec_dup (float_sqrt 2.718281828459045 float_absolute float_cbrt exec_if (exec_if () ()) ())) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 79
;;; -------------------------------------------------------
;;; Best plushy: (false float_+ boolean_= float_sqrt 2.718281828459045 boolean_= float_sqrt boolean_and boolean_positive boolean_or float_= boolean_or close float_absolute boolean_negative false 3.141592653589793 float_- boolean_positive boolean_and boolean_not boolean_= float_sqrt boolean_= float_- float_absolute float_= close float_absolute 3.141592653589793 boolean_not float_absolute)
;;; Best program: (false float_+ boolean_= float_sqrt 2.718281828459045 boolean_= float_sqrt boolean_and boolean_positive boolean_or float_= boolean_or float_absolute boolean_negative false 3.141592653589793 float_- boolean_positive boolean_and boolean_not boolean_= float_sqrt boolean_= float_- float_absolute float_= float_absolute 3.141592653589793 boolean_not float_absolute)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 80
;;; -------------------------------------------------------
;;; Best plushy: (false true boolean_not float_absolute float_+ float_absolute float_sqrt float_sqrt boolean_positive 2.718281828459045 float_* float_= float_absolute float_+ boolean_positive false in1 exec_dup boolean_= float_sqrt boolean_not float_- exec_if in1 float_cbrt close boolean_= float_+ boolean_or)
;;; Best program: (false true boolean_not float_absolute float_+ float_absolute float_sqrt float_sqrt boolean_positive 2.718281828459045 float_* float_= float_absolute float_+ boolean_positive false in1 exec_dup (boolean_= float_sqrt boolean_not float_- exec_if (in1 float_cbrt) (boolean_= float_+ boolean_or)))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 81
;;; -------------------------------------------------------
;;; Best plushy: (false boolean_positive float_cbrt float_- float_absolute close float_absolute boolean_negative float_absolute boolean_positive boolean_or float_absolute boolean_positive close exec_dup false close true float_= boolean_not in1 boolean_= float_= boolean_= float_cbrt boolean_not)
;;; Best program: (false boolean_positive float_cbrt float_- float_absolute float_absolute boolean_negative float_absolute boolean_positive boolean_or float_absolute boolean_positive exec_dup (false) true float_= boolean_not in1 boolean_= float_= boolean_= float_cbrt boolean_not)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 82
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or boolean_= close boolean_= boolean_= boolean_positive float_- float_cbrt exec_dup boolean_= boolean_not boolean_or exec_dup float_cbrt true boolean_= float_absolute boolean_negative boolean_not true float_* 3.141592653589793 boolean_not 2.718281828459045 float_cbrt close boolean_negative boolean_or in1 2.718281828459045 boolean_negative true float_* float_cbrt)
;;; Best program: (boolean_or boolean_= boolean_= boolean_= boolean_positive float_- float_cbrt exec_dup (boolean_= boolean_not boolean_or exec_dup (float_cbrt true boolean_= float_absolute boolean_negative boolean_not true float_* 3.141592653589793 boolean_not 2.718281828459045 float_cbrt) boolean_negative boolean_or in1 2.718281828459045 boolean_negative true float_* float_cbrt))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 83
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or boolean_= 3.141592653589793 in1 boolean_positive float_= in1 float_= exec_if float_= 2.718281828459045 float_absolute float_= false boolean_and boolean_negative float_= boolean_not boolean_negative true close boolean_= 3.141592653589793 true)
;;; Best program: (boolean_or boolean_= 3.141592653589793 in1 boolean_positive float_= in1 float_= exec_if (float_= 2.718281828459045 float_absolute float_= false boolean_and boolean_negative float_= boolean_not boolean_negative true) (boolean_= 3.141592653589793 true))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 84
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= boolean_= float_- boolean_= 2.718281828459045 boolean_negative boolean_or boolean_or boolean_= float_= exec_dup 2.718281828459045 float_* boolean_negative 2.718281828459045 exec_dup true float_sqrt close float_+ close boolean_negative float_absolute exec_dup exec_if float_absolute float_absolute boolean_not true)
;;; Best program: (boolean_= boolean_= float_- boolean_= 2.718281828459045 boolean_negative boolean_or boolean_or boolean_= float_= exec_dup (2.718281828459045 float_* boolean_negative 2.718281828459045 exec_dup (true float_sqrt) float_+) boolean_negative float_absolute exec_dup (exec_if (float_absolute float_absolute boolean_not true) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 85
;;; -------------------------------------------------------
;;; Best plushy: (float_= boolean_= float_+ float_absolute boolean_and float_cbrt float_= boolean_or boolean_or boolean_or 2.718281828459045 exec_dup boolean_positive exec_if boolean_positive boolean_= boolean_negative exec_if boolean_or boolean_= float_+ true false float_- float_absolute true false boolean_not float_+ float_*)
;;; Best program: (float_= boolean_= float_+ float_absolute boolean_and float_cbrt float_= boolean_or boolean_or boolean_or 2.718281828459045 exec_dup (boolean_positive exec_if (boolean_positive boolean_= boolean_negative exec_if (boolean_or boolean_= float_+ true false float_- float_absolute true false boolean_not float_+ float_*) ()) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 86
;;; -------------------------------------------------------
;;; Best plushy: (false float_cbrt float_- 2.718281828459045 2.718281828459045 boolean_or boolean_and float_absolute exec_if in1 boolean_= float_= 3.141592653589793 float_- false exec_if exec_if boolean_not float_- true float_sqrt boolean_positive 2.718281828459045 close 3.141592653589793 2.718281828459045 true)
;;; Best program: (false float_cbrt float_- 2.718281828459045 2.718281828459045 boolean_or boolean_and float_absolute exec_if (in1 boolean_= float_= 3.141592653589793 float_- false exec_if (exec_if (boolean_not float_- true float_sqrt boolean_positive 2.718281828459045) (3.141592653589793 2.718281828459045 true)) ()) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 87
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute exec_dup 3.141592653589793 2.718281828459045 boolean_= float_= close boolean_positive boolean_positive boolean_or boolean_positive exec_if exec_dup boolean_positive boolean_not boolean_= false boolean_negative exec_dup exec_if boolean_negative float_absolute float_sqrt float_= close boolean_or in1)
;;; Best program: (float_absolute exec_dup (3.141592653589793 2.718281828459045 boolean_= float_=) boolean_positive boolean_positive boolean_or boolean_positive exec_if (exec_dup (boolean_positive boolean_not boolean_= false boolean_negative exec_dup (exec_if (boolean_negative float_absolute float_sqrt float_=) (boolean_or in1)))) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 88
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= boolean_= exec_dup boolean_and boolean_not boolean_positive boolean_or true boolean_= in1 boolean_positive false false true boolean_or float_= float_sqrt float_- 3.141592653589793 2.718281828459045 float_* close float_sqrt exec_dup in1 float_absolute float_= float_*)
;;; Best program: (boolean_= boolean_= exec_dup (boolean_and boolean_not boolean_positive boolean_or true boolean_= in1 boolean_positive false false true boolean_or float_= float_sqrt float_- 3.141592653589793 2.718281828459045 float_*) float_sqrt exec_dup (in1 float_absolute float_= float_*))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 89
;;; -------------------------------------------------------
;;; Best plushy: (float_absolute boolean_and float_= false in1 boolean_and boolean_or boolean_and boolean_and false exec_if float_absolute float_+ float_sqrt 2.718281828459045 boolean_positive boolean_negative exec_dup close float_absolute false boolean_and float_cbrt float_sqrt true float_sqrt boolean_not boolean_=)
;;; Best program: (float_absolute boolean_and float_= false in1 boolean_and boolean_or boolean_and boolean_and false exec_if (float_absolute float_+ float_sqrt 2.718281828459045 boolean_positive boolean_negative exec_dup () float_absolute false boolean_and float_cbrt float_sqrt true float_sqrt boolean_not boolean_=) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 90
;;; -------------------------------------------------------
;;; Best plushy: (boolean_negative boolean_= float_- float_* in1 boolean_and boolean_or float_absolute exec_if false boolean_positive boolean_or boolean_or float_= boolean_positive float_= exec_dup float_* boolean_= close 2.718281828459045 true float_sqrt true in1)
;;; Best program: (boolean_negative boolean_= float_- float_* in1 boolean_and boolean_or float_absolute exec_if (false boolean_positive boolean_or boolean_or float_= boolean_positive float_= exec_dup (float_* boolean_=) 2.718281828459045 true float_sqrt true in1) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 91
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or boolean_= float_+ boolean_or boolean_= float_sqrt false float_sqrt exec_dup float_+ boolean_= float_* 2.718281828459045 boolean_not boolean_not float_sqrt float_sqrt 2.718281828459045 boolean_positive boolean_or float_sqrt 2.718281828459045 false float_* boolean_= boolean_= in1)
;;; Best program: (boolean_or boolean_= float_+ boolean_or boolean_= float_sqrt false float_sqrt exec_dup (float_+ boolean_= float_* 2.718281828459045 boolean_not boolean_not float_sqrt float_sqrt 2.718281828459045 boolean_positive boolean_or float_sqrt 2.718281828459045 false float_* boolean_= boolean_= in1))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 92
;;; -------------------------------------------------------
;;; Best plushy: (false boolean_or 3.141592653589793 2.718281828459045 float_cbrt float_cbrt float_= float_cbrt 2.718281828459045 exec_dup 3.141592653589793 boolean_positive true boolean_positive exec_if exec_if exec_if float_- boolean_or float_+ boolean_positive true float_absolute false false float_* true float_*)
;;; Best program: (false boolean_or 3.141592653589793 2.718281828459045 float_cbrt float_cbrt float_= float_cbrt 2.718281828459045 exec_dup (3.141592653589793 boolean_positive true boolean_positive exec_if (exec_if (exec_if (float_- boolean_or float_+ boolean_positive true float_absolute false false float_* true float_*) ()) ()) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 93
;;; -------------------------------------------------------
;;; Best plushy: (boolean_negative boolean_= false boolean_not float_- float_cbrt float_sqrt exec_dup float_- float_- float_sqrt boolean_positive true float_= float_= boolean_negative close close true float_* true float_sqrt close in1 boolean_positive boolean_or float_cbrt 2.718281828459045)
;;; Best program: (boolean_negative boolean_= false boolean_not float_- float_cbrt float_sqrt exec_dup (float_- float_- float_sqrt boolean_positive true float_= float_= boolean_negative) true float_* true float_sqrt in1 boolean_positive boolean_or float_cbrt 2.718281828459045)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 94
;;; -------------------------------------------------------
;;; Best plushy: (boolean_negative true float_- float_+ boolean_positive float_cbrt boolean_= boolean_not boolean_and boolean_negative boolean_and boolean_or float_absolute boolean_not 3.141592653589793 boolean_or float_= 3.141592653589793 boolean_positive boolean_= boolean_positive exec_dup false false exec_if float_- in1 3.141592653589793 false boolean_or boolean_and boolean_not float_absolute)
;;; Best program: (boolean_negative true float_- float_+ boolean_positive float_cbrt boolean_= boolean_not boolean_and boolean_negative boolean_and boolean_or float_absolute boolean_not 3.141592653589793 boolean_or float_= 3.141592653589793 boolean_positive boolean_= boolean_positive exec_dup (false false exec_if (float_- in1 3.141592653589793 false boolean_or boolean_and boolean_not float_absolute) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 95
;;; -------------------------------------------------------
;;; Best plushy: (boolean_or boolean_negative boolean_= boolean_negative boolean_positive 2.718281828459045 boolean_or boolean_or boolean_and boolean_and float_absolute 2.718281828459045 false 3.141592653589793 float_absolute false 3.141592653589793 2.718281828459045 2.718281828459045 2.718281828459045 true boolean_not boolean_not boolean_and 3.141592653589793 float_sqrt 3.141592653589793 boolean_and in1 boolean_negative)
;;; Best program: (boolean_or boolean_negative boolean_= boolean_negative boolean_positive 2.718281828459045 boolean_or boolean_or boolean_and boolean_and float_absolute 2.718281828459045 false 3.141592653589793 float_absolute false 3.141592653589793 2.718281828459045 2.718281828459045 2.718281828459045 true boolean_not boolean_not boolean_and 3.141592653589793 float_sqrt 3.141592653589793 boolean_and in1 boolean_negative)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 96
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= boolean_negative true boolean_negative float_- boolean_and close close float_= float_* boolean_positive true float_= float_sqrt exec_dup exec_dup close boolean_= boolean_not true float_sqrt 2.718281828459045 in1 close float_cbrt boolean_not)
;;; Best program: (boolean_= boolean_negative true boolean_negative float_- boolean_and float_= float_* boolean_positive true float_= float_sqrt exec_dup (exec_dup () boolean_= boolean_not true float_sqrt 2.718281828459045 in1) float_cbrt boolean_not)
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 97
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= boolean_not boolean_or boolean_= boolean_or 3.141592653589793 float_- boolean_and float_- float_cbrt float_= exec_if float_= float_sqrt float_cbrt exec_dup float_absolute in1 close boolean_negative close exec_if in1 float_= float_absolute boolean_or)
;;; Best program: (boolean_= boolean_not boolean_or boolean_= boolean_or 3.141592653589793 float_- boolean_and float_- float_cbrt float_= exec_if (float_= float_sqrt float_cbrt exec_dup (float_absolute in1) boolean_negative) (exec_if (in1 float_= float_absolute boolean_or) ()))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 98
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= boolean_not float_cbrt float_- boolean_or exec_if boolean_or 3.141592653589793 exec_dup exec_if boolean_negative float_* true boolean_or true float_* float_sqrt boolean_not boolean_and 2.718281828459045 exec_if boolean_not close 3.141592653589793)
;;; Best program: (boolean_= boolean_not float_cbrt float_- boolean_or exec_if (boolean_or 3.141592653589793 exec_dup (exec_if (boolean_negative float_* true boolean_or true float_* float_sqrt boolean_not boolean_and 2.718281828459045 exec_if (boolean_not) (3.141592653589793)) ())) ())
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 99
;;; -------------------------------------------------------
;;; Best plushy: (boolean_negative boolean_or float_- float_absolute boolean_and float_sqrt boolean_or boolean_= float_- true boolean_positive 3.141592653589793 float_= boolean_not boolean_not close exec_dup boolean_not float_= exec_dup boolean_= 2.718281828459045 true boolean_positive boolean_=)
;;; Best program: (boolean_negative boolean_or float_- float_absolute boolean_and float_sqrt boolean_or boolean_= float_- true boolean_positive 3.141592653589793 float_= boolean_not boolean_not exec_dup (boolean_not float_= exec_dup (boolean_= 2.718281828459045 true boolean_positive boolean_=)))
;;; Best total error: 21000000
;;; Best errors: (1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000 1000000)
;;; Best behaviors: (:no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item :no-stack-item)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 100
;;; -------------------------------------------------------
;;; Best plushy: (close float_cbrt float_cbrt float_cbrt true boolean_= float_absolute in1 boolean_or boolean_and float_absolute boolean_positive boolean_not exec_if float_= 3.141592653589793 float_absolute 3.141592653589793 exec_dup close float_* close float_cbrt float_sqrt float_= in1 float_sqrt boolean_or 2.718281828459045)
;;; Best program: (float_cbrt float_cbrt float_cbrt true boolean_= float_absolute in1 boolean_or boolean_and float_absolute boolean_positive boolean_not exec_if (float_= 3.141592653589793 float_absolute 3.141592653589793 exec_dup () float_*) (float_cbrt float_sqrt float_= in1 float_sqrt boolean_or 2.718281828459045))
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
