;; gorilla-repl.fileformat = 1

;; @@
(ns propel.core
  (:gen-class))

(def example-push-state
  {:exec '()
   :integer '(1 2 3 4 5 6 7)
   :string '("abc")
   :input {:in1 4}})

; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def default-instructions
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
   'boolean_=
   'string_=
   'string_take
   'string_drop
   'string_reverse
   'string_concat
   'string_length
   'string_includes?
   'close
   0
   1
   true
   false
   ""
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "A"
   "C"
   "G"
   "T"))

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
  [plushy instructions]
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

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
                               (:plushy (select-parent pop argmap)))
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap)))))})

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
                     (repeatedly population-size 
                                 #(new-individual evaluated-pop argmap)))))))

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
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :error-function regression-error-function
                                  :max-generations 500
                                  :population-size 200
                                  :max-initial-plushy-size 50
                                  :step-limit 100
                                  :parent-selection :tournament
                                  :tournament-size 5}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;propel.core/-main</span>","value":"#'propel.core/-main"}
;; <=

;; @@
(-main)
;; @@
;; ->
;;; Starting GP with args: {:max-initial-plushy-size 50, :mutation-rate 0.1, :instructions (in1 integer_+ integer_- integer_* integer_% integer_= exec_dup exec_if boolean_and boolean_or boolean_not boolean_= string_= string_take string_drop string_reverse string_concat string_length string_includes? close 0 1 true false  ABCDEFGHIJKLMNOPQRSTUVWXYZ A C G T), :max-generations 500, :parent-selection :tournament, :tournament-size 5, :step-limit 100, :error-function #function[propel.core/regression-error-function], :population-size 200}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= boolean_and integer_= &quot;T&quot; string_drop &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_concat boolean_= integer_+ boolean_= string_length boolean_and &quot;T&quot; integer_= string_= string_concat string_take exec_dup integer_+ string_drop boolean_not boolean_and &quot;C&quot; in1 boolean_and string_= &quot;A&quot; string_= &quot;C&quot; &quot;&quot; integer_% &quot;&quot; string_= string_= &quot;C&quot; &quot;C&quot; 0 1 boolean_not integer_= in1 integer_+)
;;; Best program: (boolean_= boolean_and integer_= &quot;T&quot; string_drop &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_concat boolean_= integer_+ boolean_= string_length boolean_and &quot;T&quot; integer_= string_= string_concat string_take exec_dup (integer_+ string_drop boolean_not boolean_and &quot;C&quot; in1 boolean_and string_= &quot;A&quot; string_= &quot;C&quot; &quot;&quot; integer_% &quot;&quot; string_= string_= &quot;C&quot; &quot;C&quot; 0 1 boolean_not integer_= in1 integer_+))
;;; Best total error: 5949
;;; Best errors: (987 717 501 333 207 117 57 21 3 3 3 3 9 27 63 123 213 339 507 723 993)
;;; Best behaviors: (-20 -18 -16 -14 -12 -10 -8 -6 -4 -2 0 2 4 6 8 10 12 14 16 18 20)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: (string_length false string_length in1 integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; true string_concat string_includes? 0 string_includes? string_length in1 true &quot;&quot; &quot;T&quot; integer_*)
;;; Best program: (string_length false string_length in1 integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; true string_concat string_includes? 0 string_includes? string_length in1 true &quot;&quot; &quot;T&quot; integer_*)
;;; Best total error: 3909
;;; Best errors: (747 501 309 165 63 3 39 51 45 27 3 21 39 45 33 3 69 171 315 507 753)
;;; Best behaviors: (-260 -234 -208 -182 -156 -130 -104 -78 -52 -26 0 26 52 78 104 130 156 182 208 234 260)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: (&quot;T&quot; in1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_length string_concat in1 close string_length &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_includes? string_reverse in1 exec_if boolean_or exec_if &quot;A&quot; boolean_= &quot;A&quot; string_= boolean_or boolean_or true string_length string_concat string_includes? boolean_and integer_*)
;;; Best program: (&quot;T&quot; in1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_length string_concat in1 string_length &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_includes? string_reverse in1 exec_if (boolean_or exec_if (&quot;A&quot; boolean_= &quot;A&quot; string_= boolean_or boolean_or true string_length string_concat string_includes? boolean_and integer_*) ()) ())
;;; Best total error: 3909
;;; Best errors: (747 501 309 165 63 3 39 51 45 27 3 21 39 45 33 3 69 171 315 507 753)
;;; Best behaviors: (-260 -234 -208 -182 -156 -130 -104 -78 -52 -26 0 26 52 78 104 130 156 182 208 234 260)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: (&quot;T&quot; close integer_% true integer_- string_= in1 exec_dup integer_- false &quot;T&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_includes? string_includes? true string_includes? 1 string_length string_includes? exec_if &quot;A&quot; boolean_= integer_* in1 exec_dup integer_+ boolean_and &quot;T&quot; string_length integer_- string_includes? boolean_and integer_*)
;;; Best program: (&quot;T&quot; integer_% true integer_- string_= in1 exec_dup (integer_- false &quot;T&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_includes? string_includes? true string_includes? 1 string_length string_includes? exec_if (&quot;A&quot; boolean_= integer_* in1 exec_dup (integer_+ boolean_and &quot;T&quot; string_length integer_- string_includes? boolean_and integer_*)) ()))
;;; Best total error: 228
;;; Best errors: (16 14 12 10 8 6 4 2 0 2 4 6 8 10 12 14 16 18 20 22 24)
;;; Best behaviors: (-991 -721 -505 -337 -211 -121 -61 -25 -7 -1 -1 -1 5 23 59 119 209 335 503 719 989)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: (string_length &quot;C&quot; exec_if true boolean_and string_= in1 exec_dup &quot;&quot; &quot;T&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop boolean_= string_includes? string_= string_includes? in1 &quot;A&quot; boolean_= integer_* in1 close boolean_or boolean_and true string_length integer_- string_includes? integer_* string_= string_= string_= integer_+)
;;; Best program: (string_length &quot;C&quot; exec_if (true boolean_and string_= in1 exec_dup (&quot;&quot; &quot;T&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop boolean_= string_includes? string_= string_includes? in1 &quot;A&quot; boolean_= integer_* in1) boolean_or boolean_and true string_length integer_- string_includes? integer_* string_= string_= string_= integer_+) ())
;;; Best total error: 833
;;; Best errors: (93 75 59 45 33 23 15 9 5 3 3 5 9 15 23 33 45 59 75 93 113)
;;; Best behaviors: (-1100 -810 -576 -392 -252 -150 -80 -36 -12 -2 0 0 4 18 48 100 180 294 448 648 900)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: (integer_* string_take integer_% boolean_= string_reverse boolean_= false in1 integer_* false close string_reverse boolean_= string_= true string_includes? boolean_= integer_- integer_+ exec_dup &quot;A&quot; in1 integer_* integer_+ &quot;&quot; &quot;T&quot; integer_-)
;;; Best program: (integer_* string_take integer_% boolean_= string_reverse boolean_= false in1 integer_* false string_reverse boolean_= string_= true string_includes? boolean_= integer_- integer_+ exec_dup (&quot;A&quot; in1 integer_* integer_+ &quot;&quot; &quot;T&quot; integer_-))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: (integer_* string_take integer_% boolean_= string_reverse boolean_= false in1 integer_* close string_reverse boolean_= string_= string_includes? integer_- integer_+ exec_dup &quot;A&quot; in1 integer_* integer_+ &quot;&quot; &quot;T&quot; integer_-)
;;; Best program: (integer_* string_take integer_% boolean_= string_reverse boolean_= false in1 integer_* string_reverse boolean_= string_= string_includes? integer_- integer_+ exec_dup (&quot;A&quot; in1 integer_* integer_+ &quot;&quot; &quot;T&quot; integer_-))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 7
;;; -------------------------------------------------------
;;; Best plushy: (integer_* string_take integer_% boolean_= string_reverse boolean_= string_= false in1 integer_* close string_reverse boolean_= string_= string_includes? integer_- integer_+ exec_dup &quot;A&quot; in1 &quot;G&quot; integer_* integer_+ &quot;&quot; &quot;T&quot; integer_-)
;;; Best program: (integer_* string_take integer_% boolean_= string_reverse boolean_= string_= false in1 integer_* string_reverse boolean_= string_= string_includes? integer_- integer_+ exec_dup (&quot;A&quot; in1 &quot;G&quot; integer_* integer_+ &quot;&quot; &quot;T&quot; integer_-))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; -------------------------------------------------------
;;; Best plushy: (integer_* string_take integer_% boolean_= string_reverse boolean_= false in1 integer_* close string_reverse boolean_= string_= string_includes? integer_- integer_+ exec_dup &quot;A&quot; in1 integer_* integer_+ &quot;&quot;)
;;; Best program: (integer_* string_take integer_% boolean_= string_reverse boolean_= false in1 integer_* string_reverse boolean_= string_= string_includes? integer_- integer_+ exec_dup (&quot;A&quot; in1 integer_* integer_+ &quot;&quot;))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 9
;;; -------------------------------------------------------
;;; Best plushy: (string_take integer_% boolean_= string_reverse boolean_= false in1 integer_* close string_reverse boolean_= string_= string_includes? integer_+ exec_dup integer_= in1 integer_* integer_+ integer_-)
;;; Best program: (string_take integer_% boolean_= string_reverse boolean_= false in1 integer_* string_reverse boolean_= string_= string_includes? integer_+ exec_dup (integer_= in1 integer_* integer_+ integer_-))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 10
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_= boolean_and true string_concat in1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; 0 integer_+ integer_+ integer_- boolean_= &quot;C&quot; string_includes? exec_dup string_reverse in1 integer_* false boolean_or string_= integer_+)
;;; Best program: (integer_% integer_= boolean_and true string_concat in1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; 0 integer_+ integer_+ integer_- boolean_= &quot;C&quot; string_includes? exec_dup (string_reverse in1 integer_* false boolean_or string_= integer_+))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 11
;;; -------------------------------------------------------
;;; Best plushy: (integer_% &quot;C&quot; in1 true string_concat &quot;A&quot; integer_* &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; 0 integer_+ integer_+ string_reverse exec_dup exec_dup boolean_= string_= boolean_= integer_+ exec_if &quot;&quot; in1 integer_*)
;;; Best program: (integer_% &quot;C&quot; in1 true string_concat &quot;A&quot; integer_* &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; 0 integer_+ integer_+ string_reverse exec_dup (exec_dup (boolean_= string_= boolean_= integer_+ exec_if (&quot;&quot; in1 integer_*) ())))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 12
;;; -------------------------------------------------------
;;; Best plushy: (integer_* string_take string_reverse boolean_= false in1 integer_* close string_length string_reverse boolean_= string_= string_includes? exec_dup &quot;A&quot; integer_= in1 integer_* integer_+ &quot;&quot;)
;;; Best program: (integer_* string_take string_reverse boolean_= false in1 integer_* string_length string_reverse boolean_= string_= string_includes? exec_dup (&quot;A&quot; integer_= in1 integer_* integer_+ &quot;&quot;))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 13
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_= boolean_and in1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; 0 integer_+ integer_+ integer_- boolean_= &quot;C&quot; string_includes? exec_dup string_reverse in1 integer_* boolean_or 1 string_= integer_+)
;;; Best program: (integer_% integer_= boolean_and in1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; 0 integer_+ integer_+ integer_- boolean_= &quot;C&quot; string_includes? exec_dup (string_reverse in1 integer_* boolean_or 1 string_= integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 14
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_= boolean_and in1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; 0 integer_+ integer_+ integer_- boolean_= &quot;C&quot; string_includes? exec_dup string_reverse in1 integer_* boolean_or 1 string_= integer_+)
;;; Best program: (integer_% integer_= boolean_and in1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; 0 integer_+ integer_+ integer_- boolean_= &quot;C&quot; string_includes? exec_dup (string_reverse in1 integer_* boolean_or 1 string_= integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 15
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_= boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup string_reverse in1 integer_* boolean_or 1 string_= integer_+)
;;; Best program: (integer_% integer_= boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup (string_reverse in1 integer_* boolean_or 1 string_= integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 16
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_= boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup string_reverse in1 integer_* boolean_or 1 string_= integer_+)
;;; Best program: (integer_% integer_= boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup (string_reverse in1 integer_* boolean_or 1 string_= integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 17
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_= boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup string_reverse in1 integer_* boolean_or 1 string_= integer_+)
;;; Best program: (integer_% integer_= boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup (string_reverse in1 integer_* boolean_or 1 string_= integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 18
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_= boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup string_reverse &quot;C&quot; in1 integer_* boolean_or 1 string_= integer_+ string_concat)
;;; Best program: (integer_% integer_= boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup (string_reverse &quot;C&quot; in1 integer_* boolean_or 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 19
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup &quot;C&quot; in1 integer_* boolean_or 1 string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup (&quot;C&quot; in1 integer_* boolean_or 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 20
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup &quot;C&quot; in1 string_reverse integer_* boolean_or 1 string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup (&quot;C&quot; in1 string_reverse integer_* boolean_or 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 21
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup &quot;C&quot; in1 string_reverse integer_* boolean_or 1 string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 0 integer_+ integer_+ integer_- boolean_= string_includes? exec_dup (&quot;C&quot; in1 string_reverse integer_* boolean_or 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 22
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 0 integer_+ integer_- string_includes? exec_dup &quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 0 integer_+ integer_- string_includes? exec_dup (&quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 23
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= integer_* string_take exec_if in1 integer_+ string_drop false &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;G&quot; boolean_not string_reverse &quot;A&quot; string_reverse integer_+ in1 boolean_and integer_* false string_length integer_+ string_reverse &quot;G&quot; integer_+ string_= boolean_and string_reverse string_includes? integer_- integer_+ integer_= in1 integer_* 1 &quot;&quot; string_= integer_+ integer_=)
;;; Best program: (boolean_= integer_* string_take exec_if (in1 integer_+ string_drop false &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;G&quot; boolean_not string_reverse &quot;A&quot; string_reverse integer_+ in1 boolean_and integer_* false string_length integer_+ string_reverse &quot;G&quot; integer_+ string_= boolean_and string_reverse string_includes? integer_- integer_+ integer_= in1 integer_* 1 &quot;&quot; string_= integer_+ integer_=) ())
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 24
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 0 integer_+ integer_- string_includes? exec_dup &quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 0 integer_+ integer_- string_includes? exec_dup (&quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 25
;;; -------------------------------------------------------
;;; Best plushy: (false integer_% boolean_and in1 0 integer_+ integer_- string_includes? &quot;T&quot; exec_dup &quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat)
;;; Best program: (false integer_% boolean_and in1 0 integer_+ integer_- string_includes? &quot;T&quot; exec_dup (&quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 26
;;; -------------------------------------------------------
;;; Best plushy: (false integer_% boolean_and in1 boolean_not 0 integer_+ integer_- string_includes? &quot;&quot; &quot;T&quot; exec_dup &quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat integer_=)
;;; Best program: (false integer_% boolean_and in1 boolean_not 0 integer_+ integer_- string_includes? &quot;&quot; &quot;T&quot; exec_dup (&quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat integer_=))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 27
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 0 integer_+ integer_- string_includes? exec_dup &quot;C&quot; in1 string_reverse integer_* 1 string_= string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 0 integer_+ integer_- string_includes? exec_dup (&quot;C&quot; in1 string_reverse integer_* 1 string_= string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 28
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and string_take in1 integer_* 0 integer_+ integer_- string_includes? exec_dup &quot;C&quot; string_= in1 string_reverse integer_* string_= 1 string_= &quot;&quot; integer_+ string_concat)
;;; Best program: (integer_% boolean_and string_take in1 integer_* 0 integer_+ integer_- string_includes? exec_dup (&quot;C&quot; string_= in1 string_reverse integer_* string_= 1 string_= &quot;&quot; integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 29
;;; -------------------------------------------------------
;;; Best plushy: (false string_= in1 0 integer_+ exec_dup &quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat)
;;; Best program: (false string_= in1 0 integer_+ exec_dup (&quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 30
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 0 integer_+ integer_- string_includes? exec_dup &quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 0 integer_+ integer_- string_includes? exec_dup (&quot;C&quot; in1 string_reverse integer_* 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 31
;;; -------------------------------------------------------
;;; Best plushy: (boolean_and in1 integer_+ exec_dup in1 string_reverse integer_* 1 string_= integer_+ string_concat)
;;; Best program: (boolean_and in1 integer_+ exec_dup (in1 string_reverse integer_* 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 32
;;; -------------------------------------------------------
;;; Best plushy: (boolean_and in1 integer_+ exec_dup in1 string_reverse boolean_= integer_* 1 string_= integer_+ string_concat)
;;; Best program: (boolean_and in1 integer_+ exec_dup (in1 string_reverse boolean_= integer_* 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 33
;;; -------------------------------------------------------
;;; Best plushy: (integer_% string_= in1 string_drop boolean_= string_includes? exec_dup in1 integer_* 1 string_= integer_+ boolean_= true string_=)
;;; Best program: (integer_% string_= in1 string_drop boolean_= string_includes? exec_dup (in1 integer_* 1 string_= integer_+ boolean_= true string_=))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 34
;;; -------------------------------------------------------
;;; Best plushy: (close integer_% boolean_and in1 0 integer_+ string_includes? integer_% exec_dup &quot;C&quot; in1 string_reverse integer_* &quot;&quot; 1 string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 0 integer_+ string_includes? integer_% exec_dup (&quot;C&quot; in1 string_reverse integer_* &quot;&quot; 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 35
;;; -------------------------------------------------------
;;; Best plushy: (string_drop integer_% string_= in1 string_drop boolean_= string_includes? exec_dup in1 boolean_and integer_* 1 string_= integer_+ boolean_= true string_=)
;;; Best program: (string_drop integer_% string_= in1 string_drop boolean_= string_includes? exec_dup (in1 boolean_and integer_* 1 string_= integer_+ boolean_= true string_=))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 36
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_% string_= in1 string_drop true boolean_= string_includes? exec_dup in1 integer_* 1 &quot;A&quot; string_= integer_+ boolean_= true true integer_% string_=)
;;; Best program: (integer_% integer_% string_= in1 string_drop true boolean_= string_includes? exec_dup (in1 integer_* 1 &quot;A&quot; string_= integer_+ boolean_= true true integer_% string_=))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 37
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 &quot;A&quot; string_concat 0 integer_+ string_includes? &quot;T&quot; exec_dup string_= integer_% &quot;C&quot; boolean_or in1 string_reverse integer_* 1 string_= integer_+ string_concat)
;;; Best program: (integer_% boolean_and in1 &quot;A&quot; string_concat 0 integer_+ string_includes? &quot;T&quot; exec_dup (string_= integer_% &quot;C&quot; boolean_or in1 string_reverse integer_* 1 string_= integer_+ string_concat))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 38
;;; -------------------------------------------------------
;;; Best plushy: (boolean_and in1 integer_+ string_drop string_includes? string_= exec_dup in1 boolean_= integer_* 1 boolean_= integer_+ string_concat string_reverse)
;;; Best program: (boolean_and in1 integer_+ string_drop string_includes? string_= exec_dup (in1 boolean_= integer_* 1 boolean_= integer_+ string_concat string_reverse))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 39
;;; -------------------------------------------------------
;;; Best plushy: (integer_% boolean_and in1 string_includes? exec_dup string_= &quot;C&quot; boolean_= string_includes? in1 integer_* 1 string_= integer_+)
;;; Best program: (integer_% boolean_and in1 string_includes? exec_dup (string_= &quot;C&quot; boolean_= string_includes? in1 integer_* 1 string_= integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 40
;;; -------------------------------------------------------
;;; Best plushy: (in1 &quot;&quot; boolean_not &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; exec_dup in1 boolean_and integer_* 1 integer_+ integer_+)
;;; Best program: (in1 &quot;&quot; boolean_not &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; exec_dup (in1 boolean_and integer_* 1 integer_+ integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 41
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; boolean_and in1 true string_includes? exec_dup &quot;C&quot; in1 integer_* 1 string_= integer_+ string_concat close integer_*)
;;; Best program: (&quot;A&quot; boolean_and in1 true string_includes? exec_dup (&quot;C&quot; in1 integer_* 1 string_= integer_+ string_concat) integer_*)
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 42
;;; -------------------------------------------------------
;;; Best plushy: (integer_% in1 integer_- exec_dup exec_if integer_- integer_= in1 string_reverse integer_* integer_- 1 string_= integer_+ integer_* integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+)
;;; Best program: (integer_% in1 integer_- exec_dup (exec_if (integer_- integer_= in1 string_reverse integer_* integer_- 1 string_= integer_+ integer_* integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+) ()))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 43
;;; -------------------------------------------------------
;;; Best plushy: (integer_% in1 string_includes? exec_dup string_= string_= string_= in1 integer_* 1 string_= integer_+ integer_+ integer_+)
;;; Best program: (integer_% in1 string_includes? exec_dup (string_= string_= string_= in1 integer_* 1 string_= integer_+ integer_+ integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 44
;;; -------------------------------------------------------
;;; Best plushy: (integer_% integer_% string_= in1 exec_if string_drop boolean_or true boolean_= string_reverse string_includes? integer_= false integer_* string_includes? integer_- exec_dup in1 integer_* string_= 1 string_= boolean_= integer_+ boolean_= exec_dup true integer_% boolean_= close exec_dup string_=)
;;; Best program: (integer_% integer_% string_= in1 exec_if (string_drop boolean_or true boolean_= string_reverse string_includes? integer_= false integer_* string_includes? integer_- exec_dup (in1 integer_* string_= 1 string_= boolean_= integer_+ boolean_= exec_dup (true integer_% boolean_=) exec_dup (string_=))) ())
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 45
;;; -------------------------------------------------------
;;; Best plushy: (in1 integer_- exec_dup integer_= boolean_not in1 string_reverse integer_* 1 string_reverse integer_+ integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;)
;;; Best program: (in1 integer_- exec_dup (integer_= boolean_not in1 string_reverse integer_* 1 string_reverse integer_+ integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 46
;;; -------------------------------------------------------
;;; Best plushy: (integer_% in1 integer_- exec_dup &quot;G&quot; integer_= in1 string_reverse integer_* integer_- 1 string_= integer_+ integer_* &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+)
;;; Best program: (integer_% in1 integer_- exec_dup (&quot;G&quot; integer_= in1 string_reverse integer_* integer_- 1 string_= integer_+ integer_* &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 47
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; boolean_and in1 string_includes? exec_dup &quot;C&quot; in1 boolean_not string_reverse integer_* 1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_= string_= integer_+ string_concat false close boolean_and integer_*)
;;; Best program: (&quot;A&quot; boolean_and in1 string_includes? exec_dup (&quot;C&quot; in1 boolean_not string_reverse integer_* 1 &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_= string_= integer_+ string_concat false) boolean_and integer_*)
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 48
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; boolean_and in1 exec_dup boolean_and string_= in1 boolean_not false integer_* integer_* 1 string_= integer_+ close string_concat integer_* string_reverse)
;;; Best program: (&quot;A&quot; boolean_and in1 exec_dup (boolean_and string_= in1 boolean_not false integer_* integer_* 1 string_= integer_+) string_concat integer_* string_reverse)
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 49
;;; -------------------------------------------------------
;;; Best plushy: (integer_% &quot;&quot; &quot;&quot; string_reverse boolean_and string_reverse string_= string_drop in1 boolean_or exec_dup in1 integer_* 1 string_= boolean_and &quot;C&quot; integer_+ &quot;T&quot; &quot;G&quot;)
;;; Best program: (integer_% &quot;&quot; &quot;&quot; string_reverse boolean_and string_reverse string_= string_drop in1 boolean_or exec_dup (in1 integer_* 1 string_= boolean_and &quot;C&quot; integer_+ &quot;T&quot; &quot;G&quot;))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 50
;;; -------------------------------------------------------
;;; Best plushy: (&quot;&quot; boolean_and string_reverse string_= boolean_and string_drop in1 exec_dup in1 boolean_and boolean_= integer_* 1 string_= string_= integer_+ integer_% string_concat &quot;G&quot; boolean_=)
;;; Best program: (&quot;&quot; boolean_and string_reverse string_= boolean_and string_drop in1 exec_dup (in1 boolean_and boolean_= integer_* 1 string_= string_= integer_+ integer_% string_concat &quot;G&quot; boolean_=))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 51
;;; -------------------------------------------------------
;;; Best plushy: (integer_% string_concat in1 integer_- exec_dup &quot;T&quot; string_= string_= in1 string_reverse integer_* boolean_not 1 exec_if string_= integer_+ integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;)
;;; Best program: (integer_% string_concat in1 integer_- exec_dup (&quot;T&quot; string_= string_= in1 string_reverse integer_* boolean_not 1 exec_if (string_= integer_+ integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;) ()))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 52
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; integer_+ boolean_and in1 boolean_and exec_dup &quot;G&quot; string_= in1 boolean_and boolean_and boolean_and integer_* 1 string_= integer_+ string_concat close boolean_and integer_*)
;;; Best program: (&quot;A&quot; integer_+ boolean_and in1 boolean_and exec_dup (&quot;G&quot; string_= in1 boolean_and boolean_and boolean_and integer_* 1 string_= integer_+ string_concat) boolean_and integer_*)
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 53
;;; -------------------------------------------------------
;;; Best plushy: (integer_= string_includes? &quot;T&quot; integer_- in1 exec_dup string_= integer_= &quot;T&quot; integer_- true in1 boolean_and integer_* 1 string_= integer_+ string_concat integer_+ exec_dup integer_+ integer_+)
;;; Best program: (integer_= string_includes? &quot;T&quot; integer_- in1 exec_dup (string_= integer_= &quot;T&quot; integer_- true in1 boolean_and integer_* 1 string_= integer_+ string_concat integer_+ exec_dup (integer_+ integer_+)))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 54
;;; -------------------------------------------------------
;;; Best plushy: (&quot;&quot; string_reverse string_= string_drop in1 exec_dup in1 integer_* 1 &quot;T&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_and integer_+ integer_% &quot;G&quot; boolean_= string_=)
;;; Best program: (&quot;&quot; string_reverse string_= string_drop in1 exec_dup (in1 integer_* 1 &quot;T&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_and integer_+ integer_% &quot;G&quot; boolean_= string_=))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 55
;;; -------------------------------------------------------
;;; Best plushy: (integer_% in1 integer_- exec_dup &quot;C&quot; string_= in1 string_reverse integer_* boolean_not 1 string_= integer_+ &quot;&quot; boolean_or integer_+ string_includes? &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;)
;;; Best program: (integer_% in1 integer_- exec_dup (&quot;C&quot; string_= in1 string_reverse integer_* boolean_not 1 string_= integer_+ &quot;&quot; boolean_or integer_+ string_includes? &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 56
;;; -------------------------------------------------------
;;; Best plushy: (&quot;&quot; exec_if integer_- &quot;&quot; boolean_and integer_- string_length in1 string_reverse boolean_= boolean_or &quot;A&quot; string_= close exec_dup in1 boolean_not &quot;A&quot; integer_* &quot;T&quot; 1 &quot;C&quot; integer_+)
;;; Best program: (&quot;&quot; exec_if (integer_- &quot;&quot; boolean_and integer_- string_length in1 string_reverse boolean_= boolean_or &quot;A&quot; string_=) (exec_dup (in1 boolean_not &quot;A&quot; integer_* &quot;T&quot; 1 &quot;C&quot; integer_+)))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 57
;;; -------------------------------------------------------
;;; Best plushy: (&quot;C&quot; integer_% boolean_and boolean_or in1 string_reverse &quot;&quot; exec_dup &quot;C&quot; string_= &quot;C&quot; false boolean_not in1 integer_* string_= integer_* string_includes? 1 boolean_not integer_+ boolean_=)
;;; Best program: (&quot;C&quot; integer_% boolean_and boolean_or in1 string_reverse &quot;&quot; exec_dup (&quot;C&quot; string_= &quot;C&quot; false boolean_not in1 integer_* string_= integer_* string_includes? 1 boolean_not integer_+ boolean_=))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 58
;;; -------------------------------------------------------
;;; Best plushy: (integer_% in1 integer_- exec_dup integer_* &quot;C&quot; string_= in1 string_reverse integer_* 1 integer_+ &quot;&quot; string_length boolean_or integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;)
;;; Best program: (integer_% in1 integer_- exec_dup (integer_* &quot;C&quot; string_= in1 string_reverse integer_* 1 integer_+ &quot;&quot; string_length boolean_or integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 59
;;; -------------------------------------------------------
;;; Best plushy: (&quot;C&quot; integer_= in1 string_reverse exec_dup string_= &quot;G&quot; &quot;C&quot; boolean_not in1 integer_* string_= integer_* 1 &quot;T&quot; integer_+ boolean_=)
;;; Best program: (&quot;C&quot; integer_= in1 string_reverse exec_dup (string_= &quot;G&quot; &quot;C&quot; boolean_not in1 integer_* string_= integer_* 1 &quot;T&quot; integer_+ boolean_=))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 60
;;; -------------------------------------------------------
;;; Best plushy: (integer_% in1 integer_- exec_dup &quot;C&quot; string_= in1 integer_* string_= 1 integer_+ &quot;&quot; integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;T&quot;)
;;; Best program: (integer_% in1 integer_- exec_dup (&quot;C&quot; string_= in1 integer_* string_= 1 integer_+ &quot;&quot; integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;T&quot;))
;;; Best total error: 42
;;; Best errors: (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;;; Best behaviors: (-1009 -737 -519 -349 -221 -129 -67 -29 -9 -1 1 3 11 31 69 131 223 351 521 739 1011)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 61
;;; -------------------------------------------------------
;;; Best plushy: (exec_if string_includes? integer_- 1 &quot;&quot; integer_- string_length in1 close &quot;&quot; boolean_= &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_or string_= close exec_dup in1 boolean_not &quot;A&quot; string_reverse integer_* &quot;T&quot; 1 exec_dup &quot;C&quot; integer_+)
;;; Best program: (exec_if (string_includes? integer_- 1 &quot;&quot; integer_- string_length in1) (&quot;&quot; boolean_= &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_or string_=) exec_dup (in1 boolean_not &quot;A&quot; string_reverse integer_* &quot;T&quot; 1 exec_dup (&quot;C&quot; integer_+)))
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 62
;;; -------------------------------------------------------
;;; Best plushy: (exec_if string_includes? integer_- 1 &quot;&quot; integer_- string_length in1 close &quot;&quot; true boolean_= &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_or string_= close exec_dup in1 boolean_not &quot;A&quot; string_reverse integer_* &quot;T&quot; boolean_not 1 exec_dup &quot;C&quot; integer_+)
;;; Best program: (exec_if (string_includes? integer_- 1 &quot;&quot; integer_- string_length in1) (&quot;&quot; true boolean_= &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_or string_=) exec_dup (in1 boolean_not &quot;A&quot; string_reverse integer_* &quot;T&quot; boolean_not 1 exec_dup (&quot;C&quot; integer_+)))
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 63
;;; -------------------------------------------------------
;;; Best plushy: (exec_if string_includes? integer_+ integer_- 1 &quot;&quot; integer_- string_length in1 close string_length &quot;&quot; 1 true boolean_= integer_* &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_or string_= close exec_dup in1 boolean_not &quot;A&quot; string_reverse integer_* &quot;T&quot; boolean_not 1 exec_dup &quot;C&quot; integer_+)
;;; Best program: (exec_if (string_includes? integer_+ integer_- 1 &quot;&quot; integer_- string_length in1) (string_length &quot;&quot; 1 true boolean_= integer_* &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_or string_=) exec_dup (in1 boolean_not &quot;A&quot; string_reverse integer_* &quot;T&quot; boolean_not 1 exec_dup (&quot;C&quot; integer_+)))
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 64
;;; -------------------------------------------------------
;;; Best plushy: (exec_if string_includes? integer_+ exec_if integer_- 1 &quot;&quot; integer_- string_length boolean_or in1 close string_length &quot;&quot; 1 true boolean_= integer_* &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_or string_= close exec_dup in1 boolean_not &quot;A&quot; string_reverse integer_* &quot;T&quot; boolean_not 1 exec_dup &quot;C&quot; integer_+)
;;; Best program: (exec_if (string_includes? integer_+ exec_if (integer_- 1 &quot;&quot; integer_- string_length boolean_or in1) (string_length &quot;&quot; 1 true boolean_= integer_* &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; boolean_or string_=) exec_dup (in1 boolean_not &quot;A&quot; string_reverse integer_* &quot;T&quot; boolean_not 1 exec_dup (&quot;C&quot; integer_+))) ())
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 65
;;; -------------------------------------------------------
;;; Best plushy: (boolean_= boolean_and &quot;T&quot; in1 string_= in1 boolean_and in1 exec_dup integer_* 1 integer_+ integer_* 1 exec_if string_concat integer_+ &quot;G&quot;)
;;; Best program: (boolean_= boolean_and &quot;T&quot; in1 string_= in1 boolean_and in1 exec_dup (integer_* 1 integer_+ integer_* 1 exec_if (string_concat integer_+ &quot;G&quot;) ()))
;;; Best total error: 0
;;; Best errors: (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;; Best behaviors: (-1007 -735 -517 -347 -219 -127 -65 -27 -7 1 3 5 13 33 71 133 225 353 523 741 1013)
;;; 
;;; SUCCESS
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
