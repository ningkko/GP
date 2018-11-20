;; gorilla-repl.fileformat = 1

;; @@
(ns propel-test
  (:gen-class))

(def example-push-state
  {:exec '()
   :integer '(1 2 3 4 5 6 7)
   :string '("abc")
   :input {:in1 4}})


; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def instructions
  (list
    'in1
    'integer_+
    'integer_-
    'integer_*
    'integer_%
    'integer_=
    'exec_dup
    'exec_if
    'boolean_and
    'boolean_or
    'boolean_not
    'boolean_is-negative
    'boolean_is-positive
    'boolean_=
    'string_=
    'string_take
    'string_drop
    'string_reverse
    'string_concat
    'string_length
    'string_absolute
    'string_includes?
    'close
    0
    1
    true
    false
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

(defn integer_+
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  [state]
  (make-push-instruction state
                         (fn [int1 int2]
                           (if (zero? int2)
                             int1
                             (quot int1 int2)))
                         [:integer :integer]
                         :integer))

(defn integer_=
  [state]
  (make-push-instruction state = [:integer :integer] :boolean))

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

(defn string_=
  [state]
  (make-push-instruction state = [:string :string] :boolean))

(defn string_take
  [state]
  (make-push-instruction state 
                         #(apply str (take %1 %2)) 
                         [:integer :string] 
                         :string))

(defn string_drop
  [state]
  (make-push-instruction state 
                         #(apply str (drop %1 %2)) 
                         [:integer :string] 
                         :string))

(defn string_reverse
  [state]
  (make-push-instruction state 
                         #(apply str (reverse %)) 
                         [:string] 
                         :string))

(defn string_concat
  [state]
  (make-push-instruction state 
                         #(apply str (concat %1 %2)) 
                         [:string :string] 
                         :string))

(defn string_length
  [state]
  (make-push-instruction state count [:string] :integer))

(defn string_includes?
  [state]
  (make-push-instruction state clojure.string/includes? [:string :string] :boolean))



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
      (integer? first-instruction) 
      (push-to-stack popped-state :integer first-instruction)
      ;
      (string? first-instruction) 
      (push-to-stack popped-state :string first-instruction)
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





;;------------------------------test-----------------------------


(defn crossover
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

   
;;-----------------------------------------------------------------




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
        inputs (range -10 11)
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

;;;;;;;;;
;; String classification

(defn string-classification-error-function
  "Finds the behaviors and errors of the individual."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        inputs ["GCG" "GACAG" "AGAAG" "CCCA" "GATTACA" "TAGG" "GACT"]
        correct-outputs [false false false false true true true]
        outputs (map (fn [input]
                       (peek-stack
                        (interpret-program 
                          program
                          (assoc empty-push-state :input {:in1 input})
                          (:step-limit argmap))
                        :boolean))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (if (= correct-output output)
                          0
                          1)))
                    correct-outputs
                    outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply +' errors))))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'propel.core)]
    (propel-gp (update-in (merge {:instructions instructions
                                  :error-function regression-error-function
                                  :max-generations 500
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
;;; Starting GP with args: {:instructions (in1 integer_+ integer_- integer_* integer_% integer_= exec_dup exec_if boolean_and boolean_or boolean_not boolean_is-negative boolean_is-positive boolean_= string_= string_take string_drop string_reverse string_concat string_length string_absolute string_includes? close 0 1 true false), :error-function #function[propel-test/regression-error-function], :max-generations 500, :population-size 200, :max-initial-plushy-size 50, :step-limit 100}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (true exec_dup integer_+ 0 boolean_or exec_dup close string_concat in1 boolean_and 1 string_length boolean_is-positive boolean_or string_reverse string_length close string_length exec_dup 1 string_= boolean_not integer_* in1 integer_* 1 1 string_concat boolean_and string_includes? boolean_and boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 boolean_or exec_dup () string_concat in1 boolean_and 1 string_length boolean_is-positive boolean_or string_reverse string_length) string_length exec_dup (1 string_= boolean_not integer_* in1 integer_* 1 1 string_concat boolean_and string_includes? boolean_and boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: (true exec_dup integer_+ 0 boolean_or exec_dup close string_concat in1 boolean_and 1 string_length boolean_is-positive boolean_or string_reverse string_length close string_length exec_dup 1 string_= boolean_not integer_* in1 integer_* 1 1 string_concat boolean_and string_includes? boolean_and boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 boolean_or exec_dup () string_concat in1 boolean_and 1 string_length boolean_is-positive boolean_or string_reverse string_length) string_length exec_dup (1 string_= boolean_not integer_* in1 integer_* 1 1 string_concat boolean_and string_includes? boolean_and boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: (true exec_dup integer_+ 0 exec_dup string_concat in1 boolean_and 1 string_length boolean_is-positive boolean_or string_reverse string_length string_length exec_dup 1 string_= boolean_not integer_* in1 integer_* 1 1 string_concat boolean_and string_includes? boolean_and boolean_or string_concat string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 exec_dup (string_concat in1 boolean_and 1 string_length boolean_is-positive boolean_or string_reverse string_length string_length exec_dup (1 string_= boolean_not integer_* in1 integer_* 1 1 string_concat boolean_and string_includes? boolean_and boolean_or string_concat string_drop string_includes? integer_% boolean_is-negative))))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: (true exec_dup integer_+ 0 boolean_or exec_dup close string_concat in1 boolean_and 1 boolean_is-positive boolean_or string_reverse string_length close exec_dup 1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 boolean_or exec_dup () string_concat in1 boolean_and 1 boolean_is-positive boolean_or string_reverse string_length) exec_dup (1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: (true exec_dup integer_+ 0 boolean_or exec_dup close string_concat in1 boolean_and 1 boolean_is-positive boolean_or string_reverse string_length close exec_dup 1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 boolean_or exec_dup () string_concat in1 boolean_and 1 boolean_is-positive boolean_or string_reverse string_length) exec_dup (1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: (true exec_dup integer_+ 0 boolean_or exec_dup close string_concat in1 boolean_and 1 boolean_is-positive boolean_or string_reverse string_length close exec_dup 1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 boolean_or exec_dup () string_concat in1 boolean_and 1 boolean_is-positive boolean_or string_reverse string_length) exec_dup (1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: (true exec_dup integer_+ 0 boolean_or exec_dup close string_concat in1 boolean_and 1 boolean_is-positive boolean_or string_reverse string_length close exec_dup 1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 boolean_or exec_dup () string_concat in1 boolean_and 1 boolean_is-positive boolean_or string_reverse string_length) exec_dup (1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; Best plushy: (true exec_dup integer_+ 0 boolean_= boolean_or exec_dup close string_concat in1 boolean_and 1 boolean_is-positive string_take boolean_or string_reverse string_length close exec_dup 1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 boolean_= boolean_or exec_dup () string_concat in1 boolean_and 1 boolean_is-positive string_take boolean_or string_reverse string_length) exec_dup (1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;; -------------------------------------------------------
;;; Best plushy: (true exec_dup integer_+ 0 boolean_= boolean_or exec_dup close string_concat in1 boolean_and 1 boolean_is-positive string_take boolean_or string_reverse string_length close exec_dup 1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true exec_dup (integer_+ 0 boolean_= boolean_or exec_dup () string_concat in1 boolean_and 1 boolean_is-positive string_take boolean_or string_reverse string_length) exec_dup (1 string_= integer_* in1 integer_* 1 1 string_concat boolean_and boolean_and boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; 
;;; -------------------------------------------------------
;;; -------------------------------------------------------
;;; Best plushy: (true integer_+ 0 boolean_or exec_dup string_concat in1 boolean_and boolean_= 1 boolean_is-positive boolean_or string_reverse string_length close exec_dup 1 string_= integer_* in1 integer_* 1 boolean_= 1 string_concat boolean_and boolean_and boolean_or boolean_or string_drop true string_concat exec_if string_drop string_includes? integer_% boolean_is-negative)
;;; Best program: (true integer_+ 0 boolean_or exec_dup (string_concat in1 boolean_and boolean_= 1 boolean_is-positive boolean_or string_reverse string_length) exec_dup (1 string_= integer_* in1 integer_* 1 boolean_= 1 string_concat boolean_and boolean_and boolean_or boolean_or string_drop true string_concat exec_if (string_drop string_includes? integer_% boolean_is-negative) ()))
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;; -------------------------------------------------------
;;; Best plushy: (string_reverse close in1 exec_if in1 in1 exec_dup string_length boolean_or integer_* integer_* close exec_dup integer_+ integer_+ integer_+ in1 integer_+ integer_+)
;;; Best program: (string_reverse in1 exec_if (in1 in1 exec_dup (string_length boolean_or integer_* integer_*) exec_dup (integer_+ integer_+ integer_+ in1 integer_+ integer_+)) ())
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 12
;;;                Report for Generation 13
;;; Best plushy: (in1 in1 in1 in1 exec_dup exec_dup string_drop string_drop integer_* integer_* boolean_or boolean_or integer_+ integer_+ boolean_not integer_+ string_take)
;;; Best program: (in1 in1 in1 in1 exec_dup (exec_dup (string_drop string_drop integer_* integer_* boolean_or boolean_or integer_+ integer_+ boolean_not integer_+ string_take)))
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 15
;;;                Report for Generation 17
;;; Best plushy: (in1 in1 in1 in1 exec_dup exec_dup string_drop string_drop integer_* integer_* boolean_or string_reverse string_concat string_take integer_+ string_concat integer_+ integer_+)
;;; Best program: (in1 in1 in1 in1 exec_dup (exec_dup (string_drop string_drop integer_* integer_* boolean_or string_reverse string_concat string_take integer_+ string_concat integer_+ integer_+)))
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; -------------------------------------------------------
;;; Best plushy: (close in1 1 in1 in1 in1 exec_dup exec_dup string_drop string_length integer_* close boolean_or integer_+ string_concat true integer_+ string_take)
;;; Best program: (in1 1 in1 in1 in1 exec_dup (exec_dup (string_drop string_length integer_*) boolean_or integer_+ string_concat true integer_+ string_take))
;;;                Report for Generation 19
;;; -------------------------------------------------------
;;; Best plushy: (in1 in1 in1 in1 string_drop string_drop exec_dup exec_dup boolean_or boolean_or integer_* integer_* boolean_not string_take integer_+ string_take)
;;; Best program: (in1 in1 in1 in1 string_drop string_drop exec_dup (exec_dup (boolean_or boolean_or integer_* integer_* boolean_not string_take integer_+ string_take)))
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;;                Report for Generation 20
;;; Best plushy: (string_reverse string_reverse in1 in1 in1 in1 boolean_and exec_dup string_length integer_* boolean_or exec_if integer_* exec_dup integer_+ integer_+ 1 string_concat integer_+ integer_+ string_take)
;;; Best program: (string_reverse string_reverse in1 in1 in1 in1 boolean_and exec_dup (string_length integer_* boolean_or exec_if (integer_* exec_dup (integer_+ integer_+ 1 string_concat integer_+ integer_+ string_take)) ()))
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1006 -734 -516 -346 -218 -126 -64 -26 -6 2 4 6 14 34 72 134 226 354 524 742 1014)
;;; 
;;; -------------------------------------------------------
;;; Best plushy: (string_reverse string_reverse in1 in1 in1 in1 boolean_and exec_dup string_length integer_* boolean_or exec_if integer_* exec_dup integer_+ integer_+ 1 string_concat integer_+ integer_+ string_take)
;;; Best program: (string_reverse string_reverse in1 in1 in1 in1 boolean_and exec_dup (string_length integer_* boolean_or exec_if (integer_* exec_dup (integer_+ integer_+ 1 string_concat integer_+ integer_+ string_take)) ()))
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1006 -734 -516 -346 -218 -126 -64 -26 -6 2 4 6 14 34 72 134 226 354 524 742 1014)
;;; 
;;; -------------------------------------------------------
;;; -------------------------------------------------------
;;; Best plushy: (in1 in1 in1 in1 exec_dup exec_dup string_drop string_drop integer_* integer_* 1 boolean_or boolean_or integer_+ integer_+ integer_+ integer_+)
;;; Best program: (in1 in1 in1 in1 exec_dup (exec_dup (string_drop string_drop integer_* integer_* 1 boolean_or boolean_or integer_+ integer_+ integer_+ integer_+)))
;;;                Report for Generation 24
;;; Best plushy: (in1 in1 in1 in1 exec_dup exec_dup string_drop integer_* integer_* boolean_or 1 string_take boolean_or string_concat integer_+ integer_+)
;;; Best program: (in1 in1 in1 in1 exec_dup (exec_dup (string_drop integer_* integer_* boolean_or 1 string_take boolean_or string_concat integer_+ integer_+)))
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1006 -734 -516 -346 -218 -126 -64 -26 -6 2 4 6 14 34 72 134 226 354 524 742 1014)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 26
;;; Best plushy: (close in1 in1 in1 in1 exec_dup string_drop integer_* integer_* 1 boolean_or integer_+ integer_+ integer_+ integer_+)
;;; Best program: (in1 in1 in1 in1 exec_dup (string_drop integer_* integer_* 1 boolean_or integer_+ integer_+ integer_+ integer_+))
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;;                Report for Generation 27
;;;                Report for Generation 28
;;; 
;; <-

;; @@

;; @@

;; @@

;; @@
