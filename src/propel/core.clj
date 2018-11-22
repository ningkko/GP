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
    "T"
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;propel.core/-main</span>","value":"#'propel.core/-main"}
;; <=

;; @@
instructions
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>in1</span>","value":"in1"},{"type":"html","content":"<span class='clj-symbol'>integer_+</span>","value":"integer_+"},{"type":"html","content":"<span class='clj-symbol'>integer_-</span>","value":"integer_-"},{"type":"html","content":"<span class='clj-symbol'>integer_*</span>","value":"integer_*"},{"type":"html","content":"<span class='clj-symbol'>integer_%</span>","value":"integer_%"},{"type":"html","content":"<span class='clj-symbol'>integer_=</span>","value":"integer_="},{"type":"html","content":"<span class='clj-symbol'>exec_dup</span>","value":"exec_dup"},{"type":"html","content":"<span class='clj-symbol'>exec_if</span>","value":"exec_if"},{"type":"html","content":"<span class='clj-symbol'>boolean_and</span>","value":"boolean_and"},{"type":"html","content":"<span class='clj-symbol'>boolean_or</span>","value":"boolean_or"},{"type":"html","content":"<span class='clj-symbol'>boolean_not</span>","value":"boolean_not"},{"type":"html","content":"<span class='clj-symbol'>boolean_=</span>","value":"boolean_="},{"type":"html","content":"<span class='clj-symbol'>string_=</span>","value":"string_="},{"type":"html","content":"<span class='clj-symbol'>string_take</span>","value":"string_take"},{"type":"html","content":"<span class='clj-symbol'>string_drop</span>","value":"string_drop"},{"type":"html","content":"<span class='clj-symbol'>string_reverse</span>","value":"string_reverse"},{"type":"html","content":"<span class='clj-symbol'>string_concat</span>","value":"string_concat"},{"type":"html","content":"<span class='clj-symbol'>string_length</span>","value":"string_length"},{"type":"html","content":"<span class='clj-symbol'>string_includes?</span>","value":"string_includes?"},{"type":"html","content":"<span class='clj-symbol'>close</span>","value":"close"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-string'>&quot;&quot;</span>","value":"\"\""},{"type":"html","content":"<span class='clj-string'>&quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot;</span>","value":"\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\""},{"type":"html","content":"<span class='clj-string'>&quot;A&quot;</span>","value":"\"A\""},{"type":"html","content":"<span class='clj-string'>&quot;C&quot;</span>","value":"\"C\""},{"type":"html","content":"<span class='clj-string'>&quot;G&quot;</span>","value":"\"G\""},{"type":"html","content":"<span class='clj-string'>&quot;T&quot;</span>","value":"\"T\""}],"value":"(in1 integer_+ integer_- integer_* integer_% integer_= exec_dup exec_if boolean_and boolean_or boolean_not boolean_= string_= string_take string_drop string_reverse string_concat string_length string_includes? close 0 1 true false \"\" \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\" \"A\" \"C\" \"G\" \"T\")"}
;; <=

;; @@
(-main)
;; @@
;; ->
;;; Starting GP with args: {:instructions (in1 integer_+ integer_- integer_* integer_% integer_= exec_dup exec_if boolean_and boolean_or boolean_not boolean_= string_= string_take string_drop string_reverse string_concat string_length string_includes? close 0 1 true false  ABCDEFGHIJKLMNOPQRSTUVWXYZ A C G T), :error-function #function[propel.core/regression-error-function], :max-generations 500, :population-size 200, :max-initial-plushy-size 50, :step-limit 100}
;;; -------------------------------------------------------
;;;                Report for Generation 0
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; &quot;G&quot; boolean_or 1 string_drop 1 integer_- string_includes? boolean_not in1 0 string_take integer_% integer_+ &quot;G&quot; string_= boolean_not 0 integer_% &quot;G&quot; string_includes? string_reverse integer_% exec_if integer_- integer_+ exec_if string_reverse boolean_not integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_- 1 &quot;&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_% integer_= integer_+ &quot;C&quot; integer_- integer_%)
;;; Best program: (true &quot;T&quot; &quot;G&quot; boolean_or 1 string_drop 1 integer_- string_includes? boolean_not in1 0 string_take integer_% integer_+ &quot;G&quot; string_= boolean_not 0 integer_% &quot;G&quot; string_includes? string_reverse integer_% exec_if (integer_- integer_+ exec_if (string_reverse boolean_not integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_- 1 &quot;&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_% integer_= integer_+ &quot;C&quot; integer_- integer_%) ()) ())
;;; Best total error: 6054
;;; Best errors: (998 727 510 341 214 123 62 25 6 1 2 3 10 29 66 127 218 345 514 731 1002)
;;; Best behaviors: (-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 1
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; &quot;G&quot; integer_= 1 string_drop 1 string_drop false in1 in1 0 false string_length integer_+ boolean_and string_= boolean_not integer_% &quot;G&quot; exec_if integer_+ exec_if string_reverse boolean_not &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;&quot; integer_% integer_= integer_+ &quot;C&quot; integer_- integer_%)
;;; Best program: (true &quot;T&quot; &quot;G&quot; integer_= 1 string_drop 1 string_drop false in1 in1 0 false string_length integer_+ boolean_and string_= boolean_not integer_% &quot;G&quot; exec_if (integer_+ exec_if (string_reverse boolean_not &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;&quot; integer_% integer_= integer_+ &quot;C&quot; integer_- integer_%) ()) ())
;;; Best total error: 5949
;;; Best errors: (987 717 501 333 207 117 57 21 3 3 3 3 9 27 63 123 213 339 507 723 993)
;;; Best behaviors: (-20 -18 -16 -14 -12 -10 -8 -6 -4 -2 0 2 4 6 8 10 12 14 16 18 20)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 2
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; integer_+ string_drop exec_dup in1 0 boolean_or string_drop integer_- integer_+ &quot;&quot; exec_dup boolean_= string_reverse integer_= integer_- string_length boolean_= &quot;C&quot; 1 close integer_* in1 integer_+ close integer_+ boolean_and)
;;; Best program: (&quot;A&quot; integer_+ string_drop exec_dup (in1 0 boolean_or string_drop integer_- integer_+ &quot;&quot; exec_dup (boolean_= string_reverse integer_= integer_- string_length boolean_= &quot;C&quot; 1) integer_* in1 integer_+) integer_+ boolean_and)
;;; Best total error: 5943
;;; Best errors: (989 719 503 335 209 119 59 23 5 1 1 1 7 25 61 121 211 337 505 721 991)
;;; Best behaviors: (-18 -16 -14 -12 -10 -8 -6 -4 -2 0 2 4 6 8 10 12 14 16 18 20 22)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 3
;;; -------------------------------------------------------
;;; Best plushy: (close &quot;G&quot; true true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop close string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat string_= 1 &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* 0 in1 string_= integer_+ integer_*)
;;; Best program: (&quot;G&quot; true true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat string_= 1 &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* 0 in1 string_= integer_+ integer_*)
;;; Best total error: 3973
;;; Best errors: (757 510 317 172 69 2 35 48 43 26 3 20 37 42 29 8 75 178 323 516 763)
;;; Best behaviors: (-250 -225 -200 -175 -150 -125 -100 -75 -50 -25 0 25 50 75 100 125 150 175 200 225 250)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 4
;;; -------------------------------------------------------
;;; Best plushy: (close &quot;G&quot; true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop close string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat string_= 1 &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* 0 in1 string_= integer_+ integer_*)
;;; Best program: (&quot;G&quot; true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat string_= 1 &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* 0 in1 string_= integer_+ integer_*)
;;; Best total error: 3973
;;; Best errors: (757 510 317 172 69 2 35 48 43 26 3 20 37 42 29 8 75 178 323 516 763)
;;; Best behaviors: (-250 -225 -200 -175 -150 -125 -100 -75 -50 -25 0 25 50 75 100 125 150 175 200 225 250)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 5
;;; -------------------------------------------------------
;;; Best plushy: (close &quot;G&quot; true &quot;T&quot; string_length &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop close string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat string_= 1 &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* 0 in1 string_= integer_+ integer_*)
;;; Best program: (&quot;G&quot; true &quot;T&quot; string_length &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat string_= 1 &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* 0 in1 string_= integer_+ integer_*)
;;; Best total error: 3973
;;; Best errors: (757 510 317 172 69 2 35 48 43 26 3 20 37 42 29 8 75 178 323 516 763)
;;; Best behaviors: (-250 -225 -200 -175 -150 -125 -100 -75 -50 -25 0 25 50 75 100 125 150 175 200 225 250)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 6
;;; -------------------------------------------------------
;;; Best plushy: (close &quot;G&quot; true &quot;T&quot; string_length &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop close string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat string_= 1 &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* 0 in1 string_= integer_+ integer_*)
;;; Best program: (&quot;G&quot; true &quot;T&quot; string_length &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat string_= 1 &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* 0 in1 string_= integer_+ integer_*)
;;; Best total error: 3973
;;; Best errors: (757 510 317 172 69 2 35 48 43 26 3 20 37 42 29 8 75 178 323 516 763)
;;; Best behaviors: (-250 -225 -200 -175 -150 -125 -100 -75 -50 -25 0 25 50 75 100 125 150 175 200 225 250)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 7
;;; -------------------------------------------------------
;;; Best plushy: (close &quot;G&quot; true string_concat &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop close string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat 1 &quot;C&quot; string_includes? &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* string_= 0 in1 integer_+ &quot;G&quot; integer_*)
;;; Best program: (&quot;G&quot; true string_concat &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;T&quot; string_reverse string_includes? boolean_not boolean_or &quot;G&quot; boolean_not string_concat 1 &quot;C&quot; string_includes? &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; integer_* string_= 0 in1 integer_+ &quot;G&quot; integer_*)
;;; Best total error: 3973
;;; Best errors: (757 510 317 172 69 2 35 48 43 26 3 20 37 42 29 8 75 178 323 516 763)
;;; Best behaviors: (-250 -225 -200 -175 -150 -125 -100 -75 -50 -25 0 25 50 75 100 125 150 175 200 225 250)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 8
;;; -------------------------------------------------------
;;; Best plushy: (true string_concat &quot;T&quot; string_concat &quot;T&quot; true integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop close string_length &quot;T&quot; string_reverse string_reverse string_includes? boolean_not boolean_or &quot;G&quot; string_concat string_concat &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_= 0 in1 string_= integer_+ &quot;G&quot; integer_* integer_*)
;;; Best program: (true string_concat &quot;T&quot; string_concat &quot;T&quot; true integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;T&quot; string_reverse string_reverse string_includes? boolean_not boolean_or &quot;G&quot; string_concat string_concat &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_= 0 in1 string_= integer_+ &quot;G&quot; integer_* integer_*)
;;; Best total error: 3909
;;; Best errors: (747 501 309 165 63 3 39 51 45 27 3 21 39 45 33 3 69 171 315 507 753)
;;; Best behaviors: (-260 -234 -208 -182 -156 -130 -104 -78 -52 -26 0 26 52 78 104 130 156 182 208 234 260)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 9
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; integer_+ string_drop exec_dup in1 0 0 string_drop string_drop integer_- integer_+ &quot;&quot; integer_% exec_dup exec_dup string_reverse integer_= in1 string_length false &quot;C&quot; string_= close integer_* in1 integer_+ close string_reverse boolean_and)
;;; Best program: (&quot;A&quot; integer_+ string_drop exec_dup (in1 0 0 string_drop string_drop integer_- integer_+ &quot;&quot; integer_% exec_dup (exec_dup (string_reverse integer_= in1 string_length false &quot;C&quot; string_=) integer_* in1 integer_+) string_reverse boolean_and))
;;; Best total error: 1487
;;; Best errors: (207 168 133 102 75 52 33 18 7 0 3 2 3 12 25 42 63 88 117 150 187)
;;; Best behaviors: (-800 -567 -384 -245 -144 -75 -32 -9 0 1 0 3 16 45 96 175 288 441 640 891 1200)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 10
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; integer_+ string_drop exec_dup in1 0 0 string_drop string_drop integer_- integer_+ &quot;&quot; integer_% exec_dup string_reverse integer_= in1 string_length false &quot;C&quot; string_= close integer_* in1 integer_+ string_reverse boolean_and)
;;; Best program: (&quot;A&quot; integer_+ string_drop exec_dup (in1 0 0 string_drop string_drop integer_- integer_+ &quot;&quot; integer_% exec_dup (string_reverse integer_= in1 string_length false &quot;C&quot; string_=) integer_* in1 integer_+ string_reverse boolean_and))
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 11
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; integer_+ string_drop exec_dup in1 0 0 string_drop string_drop integer_- &quot;&quot; integer_% exec_dup string_reverse integer_= in1 string_length false &quot;C&quot; string_= close integer_* in1 integer_+ string_reverse boolean_and)
;;; Best program: (&quot;A&quot; integer_+ string_drop exec_dup (in1 0 0 string_drop string_drop integer_- &quot;&quot; integer_% exec_dup (string_reverse integer_= in1 string_length false &quot;C&quot; string_=) integer_* in1 integer_+ string_reverse boolean_and))
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 12
;;; -------------------------------------------------------
;;; Best plushy: (&quot;A&quot; integer_+ string_drop exec_dup in1 boolean_and 0 0 string_drop string_drop integer_- true &quot;&quot; integer_% exec_dup string_reverse integer_= in1 string_length false &quot;C&quot; string_= close integer_* in1 integer_+ string_reverse boolean_and)
;;; Best program: (&quot;A&quot; integer_+ string_drop exec_dup (in1 boolean_and 0 0 string_drop string_drop integer_- true &quot;&quot; integer_% exec_dup (string_reverse integer_= in1 string_length false &quot;C&quot; string_=) integer_* in1 integer_+ string_reverse boolean_and))
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 13
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close string_drop string_length &quot;&quot; integer_% close string_includes? boolean_not integer_= string_drop integer_- boolean_= &quot;C&quot; true &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; in1 integer_+ exec_dup boolean_and in1 string_= &quot;G&quot; integer_*)
;;; Best program: (true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;&quot; integer_% string_includes? boolean_not integer_= string_drop integer_- boolean_= &quot;C&quot; true &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; in1 integer_+ exec_dup (boolean_and in1 string_= &quot;G&quot; integer_*))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 14
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close string_drop string_length &quot;&quot; integer_% close string_includes? boolean_not boolean_= integer_= string_drop integer_- boolean_= &quot;C&quot; true &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; in1 integer_+ exec_dup boolean_and in1 string_= &quot;G&quot; integer_*)
;;; Best program: (true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;&quot; integer_% string_includes? boolean_not boolean_= integer_= string_drop integer_- boolean_= &quot;C&quot; true &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; in1 integer_+ exec_dup (boolean_and in1 string_= &quot;G&quot; integer_*))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 15
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close string_drop string_length &quot;&quot; integer_% close string_includes? boolean_not boolean_= integer_= string_drop integer_- boolean_= &quot;C&quot; true &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; in1 integer_+ exec_dup boolean_and in1 string_= &quot;G&quot; integer_*)
;;; Best program: (true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop string_length &quot;&quot; integer_% string_includes? boolean_not boolean_= integer_= string_drop integer_- boolean_= &quot;C&quot; true &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; in1 integer_+ exec_dup (boolean_and in1 string_= &quot;G&quot; integer_*))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 16
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close string_drop &quot;&quot; integer_% close string_includes? boolean_not boolean_= integer_= string_drop integer_- boolean_= &quot;C&quot; true &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; in1 integer_+ exec_dup boolean_and in1 string_= &quot;G&quot; integer_*)
;;; Best program: (true &quot;T&quot; true string_length integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop &quot;&quot; integer_% string_includes? boolean_not boolean_= integer_= string_drop integer_- boolean_= &quot;C&quot; true &quot;C&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; in1 integer_+ exec_dup (boolean_and in1 string_= &quot;G&quot; integer_*))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 17
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;C&quot; &quot;T&quot; true integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_length string_drop integer_% string_includes? string_includes? boolean_not boolean_= boolean_not string_drop integer_- boolean_= &quot;C&quot; integer_+ boolean_or boolean_and in1 string_= exec_dup integer_* in1 integer_* boolean_and)
;;; Best program: (true &quot;C&quot; &quot;T&quot; true integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_length string_drop integer_% string_includes? string_includes? boolean_not boolean_= boolean_not string_drop integer_- boolean_= &quot;C&quot; integer_+ boolean_or boolean_and in1 string_= exec_dup (integer_* in1 integer_* boolean_and))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 18
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; true string_length integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close string_take string_drop integer_+ &quot;&quot; boolean_= string_includes? boolean_not string_reverse integer_= string_drop string_length integer_- &quot;C&quot; true &quot;C&quot; in1 in1 in1 close in1 in1 integer_* in1 string_= integer_*)
;;; Best program: (true &quot;T&quot; true string_length integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_take string_drop integer_+ &quot;&quot; boolean_= string_includes? boolean_not string_reverse integer_= string_drop string_length integer_- &quot;C&quot; true &quot;C&quot; in1 in1 in1 in1 in1 integer_* in1 string_= integer_*)
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 19
;;; -------------------------------------------------------
;;; Best plushy: (string_concat true &quot;T&quot; true string_length integer_+ close string_drop &quot;&quot; integer_% string_includes? boolean_not boolean_= integer_= string_drop boolean_= &quot;C&quot; true in1 0 integer_+ exec_dup boolean_and in1 string_= &quot;G&quot; integer_*)
;;; Best program: (string_concat true &quot;T&quot; true string_length integer_+ string_drop &quot;&quot; integer_% string_includes? boolean_not boolean_= integer_= string_drop boolean_= &quot;C&quot; true in1 0 integer_+ exec_dup (boolean_and in1 string_= &quot;G&quot; integer_*))
;;; Best total error: 119
;;; Best errors: (7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
;;; Best behaviors: (-1000 -729 -512 -343 -216 -125 -64 -27 -8 -1 0 1 8 27 64 125 216 343 512 729 1000)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 20
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; close true string_length integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close string_drop integer_+ &quot;&quot; boolean_= string_includes? boolean_not string_reverse integer_= string_drop string_length integer_- &quot;C&quot; true &quot;C&quot; in1 in1 close in1 integer_* in1 string_= integer_* integer_+)
;;; Best program: (true &quot;T&quot; true string_length integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop integer_+ &quot;&quot; boolean_= string_includes? boolean_not string_reverse integer_= string_drop string_length integer_- &quot;C&quot; true &quot;C&quot; in1 in1 in1 integer_* in1 string_= integer_* integer_+)
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 21
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;C&quot; close true integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_% string_includes? &quot;&quot; boolean_= string_includes? string_drop string_drop integer_- true string_length integer_- in1 true exec_dup boolean_and in1 close in1 integer_* boolean_and integer_* integer_+)
;;; Best program: (true &quot;C&quot; true integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_% string_includes? &quot;&quot; boolean_= string_includes? string_drop string_drop integer_- true string_length integer_- in1 true exec_dup (boolean_and in1) in1 integer_* boolean_and integer_* integer_+)
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 22
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; close true string_length integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close string_drop integer_+ &quot;&quot; integer_= boolean_= string_includes? boolean_not string_reverse string_= integer_= string_drop string_length integer_- &quot;C&quot; true &quot;C&quot; in1 in1 close in1 integer_* in1 string_= integer_* integer_+)
;;; Best program: (true &quot;T&quot; true string_length integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop integer_+ &quot;&quot; integer_= boolean_= string_includes? boolean_not string_reverse string_= integer_= string_drop string_length integer_- &quot;C&quot; true &quot;C&quot; in1 in1 in1 integer_* in1 string_= integer_* integer_+)
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 23
;;; -------------------------------------------------------
;;; Best plushy: (true &quot;T&quot; close true string_length integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close string_drop integer_+ &quot;&quot; integer_= boolean_= string_includes? boolean_not string_reverse string_= integer_= string_drop string_length integer_- &quot;C&quot; true &quot;C&quot; in1 in1 close in1 integer_* in1 string_= integer_* integer_+)
;;; Best program: (true &quot;T&quot; true string_length integer_% &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; string_drop integer_+ &quot;&quot; integer_= boolean_= string_includes? boolean_not string_reverse string_= integer_= string_drop string_length integer_- &quot;C&quot; true &quot;C&quot; in1 in1 in1 integer_* in1 string_= integer_* integer_+)
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 24
;;; -------------------------------------------------------
;;; Best plushy: (true close true integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_% string_includes? &quot;&quot; boolean_= string_includes? string_drop string_drop integer_- true string_length integer_- in1 true exec_dup boolean_and in1 close in1 integer_* string_= boolean_and integer_* integer_+)
;;; Best program: (true true integer_+ &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_% string_includes? &quot;&quot; boolean_= string_includes? string_drop string_drop integer_- true string_length integer_- in1 true exec_dup (boolean_and in1) in1 integer_* string_= boolean_and integer_* integer_+)
;;; Best total error: 63
;;; Best errors: (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;; Best behaviors: (-1010 -738 -520 -350 -222 -130 -68 -30 -10 -2 0 2 10 30 68 130 222 350 520 738 1010)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 25
;;; -------------------------------------------------------
;;; Best plushy: (0 true &quot;C&quot; &quot;G&quot; close string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 true exec_dup boolean_and in1 close &quot;G&quot; integer_* integer_* integer_+)
;;; Best program: (0 true &quot;C&quot; &quot;G&quot; string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 true exec_dup (boolean_and in1) &quot;G&quot; integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 26
;;; -------------------------------------------------------
;;; Best plushy: (0 true &quot;C&quot; &quot;G&quot; close string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 true exec_dup boolean_and in1 close &quot;G&quot; integer_* integer_* integer_+)
;;; Best program: (0 true &quot;C&quot; &quot;G&quot; string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 true exec_dup (boolean_and in1) &quot;G&quot; integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 27
;;; -------------------------------------------------------
;;; Best plushy: (0 string_drop true &quot;C&quot; &quot;G&quot; close string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 true exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+)
;;; Best program: (0 string_drop true &quot;C&quot; &quot;G&quot; string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 true exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 28
;;; -------------------------------------------------------
;;; Best plushy: (0 string_drop true &quot;C&quot; &quot;G&quot; close string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (0 string_drop true &quot;C&quot; &quot;G&quot; string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 29
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; close string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 30
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; boolean_and close string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; boolean_and string_concat true integer_+ true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 31
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; close string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 32
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; close string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ string_concat in1 string_drop true exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 33
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; close string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse integer_+ string_concat in1 string_drop true exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop &quot;&quot; in1 string_includes? string_length string_drop string_reverse integer_+ string_concat in1 string_drop true exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 34
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; close string_concat true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop integer_+ &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ in1 string_drop true exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; string_concat true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop integer_+ &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ in1 string_drop true exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 35
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; close string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close false integer_+ string_drop string_drop &quot;&quot; in1 integer_- string_includes? string_length string_drop string_reverse true integer_+ string_concat true in1 string_drop true exec_dup boolean_and in1 string_length close string_take &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; false integer_+ string_drop string_drop &quot;&quot; in1 integer_- string_includes? string_length string_drop string_reverse true integer_+ string_concat true in1 string_drop true exec_dup (boolean_and in1 string_length) string_take &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 36
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; close string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close false integer_+ string_drop string_drop &quot;&quot; in1 integer_- string_includes? string_length string_drop string_reverse true integer_+ string_concat true in1 string_drop exec_dup boolean_and in1 string_length close &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; string_concat true true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; false integer_+ string_drop string_drop &quot;&quot; in1 integer_- string_includes? string_length string_drop string_reverse true integer_+ string_concat true in1 string_drop exec_dup (boolean_and in1 string_length) &quot;G&quot; integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 37
;;; -------------------------------------------------------
;;; Best plushy: (string_drop true &quot;C&quot; &quot;G&quot; close string_concat true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; close integer_+ string_drop integer_+ &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ in1 string_drop true exec_dup boolean_and string_reverse in1 string_length close &quot;G&quot; boolean_not integer_* integer_* integer_+ string_includes?)
;;; Best program: (string_drop true &quot;C&quot; &quot;G&quot; string_concat true &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ string_drop integer_+ &quot;&quot; in1 string_includes? string_length string_drop string_reverse true integer_+ in1 string_drop true exec_dup (boolean_and string_reverse in1 string_length) &quot;G&quot; boolean_not integer_* integer_* integer_+ string_includes?)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 38
;;; -------------------------------------------------------
;;; Best plushy: (close close string_concat string_take true integer_+ string_drop integer_+ integer_= integer_+ string_includes? 1 in1 1 string_length integer_+ string_drop string_reverse &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ in1 true exec_dup in1 boolean_or close &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best program: (string_concat string_take true integer_+ string_drop integer_+ integer_= integer_+ string_includes? 1 in1 1 string_length integer_+ string_drop string_reverse &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ in1 true exec_dup (in1 boolean_or) &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 39
;;; -------------------------------------------------------
;;; Best plushy: (close close string_concat string_take integer_+ string_drop integer_+ integer_= integer_+ string_includes? 1 in1 1 string_length integer_+ string_drop string_reverse &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ in1 true exec_dup in1 boolean_or close &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best program: (string_concat string_take integer_+ string_drop integer_+ integer_= integer_+ string_includes? 1 in1 1 string_length integer_+ string_drop string_reverse &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ in1 true exec_dup (in1 boolean_or) &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 40
;;; -------------------------------------------------------
;;; Best plushy: (close close string_concat string_take integer_+ string_drop integer_+ integer_= integer_+ string_includes? 1 in1 1 string_length integer_+ string_drop string_reverse &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ in1 true exec_dup in1 boolean_or close &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best program: (string_concat string_take integer_+ string_drop integer_+ integer_= integer_+ string_includes? 1 in1 1 string_length integer_+ string_drop string_reverse &quot;ABCDEFGHIJKLMNOPQRSTUVWXYZ&quot; integer_+ in1 true exec_dup (in1 boolean_or) &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 41
;;; -------------------------------------------------------
;;; Best plushy: (true close close string_concat string_take integer_+ string_drop integer_+ integer_= integer_+ string_includes? string_length 1 integer_+ in1 1 integer_+ string_reverse integer_+ &quot;C&quot; true in1 true exec_dup in1 boolean_or close &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best program: (true string_concat string_take integer_+ string_drop integer_+ integer_= integer_+ string_includes? string_length 1 integer_+ in1 1 integer_+ string_reverse integer_+ &quot;C&quot; true in1 true exec_dup (in1 boolean_or) &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 42
;;; -------------------------------------------------------
;;; Best plushy: (true close close string_concat string_take integer_+ integer_+ integer_= integer_+ string_includes? 1 integer_+ in1 1 integer_+ string_reverse string_take integer_+ &quot;C&quot; true in1 true &quot;&quot; exec_dup in1 &quot;G&quot; boolean_or close string_reverse &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best program: (true string_concat string_take integer_+ integer_+ integer_= integer_+ string_includes? 1 integer_+ in1 1 integer_+ string_reverse string_take integer_+ &quot;C&quot; true in1 true &quot;&quot; exec_dup (in1 &quot;G&quot; boolean_or) string_reverse &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 43
;;; -------------------------------------------------------
;;; Best plushy: (true close close string_concat string_take integer_+ integer_+ integer_= integer_+ string_includes? 1 integer_+ in1 1 integer_+ string_reverse string_take integer_+ &quot;C&quot; true in1 true &quot;&quot; exec_dup in1 &quot;G&quot; boolean_or close string_reverse &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best program: (true string_concat string_take integer_+ integer_+ integer_= integer_+ string_includes? 1 integer_+ in1 1 integer_+ string_reverse string_take integer_+ &quot;C&quot; true in1 true &quot;&quot; exec_dup (in1 &quot;G&quot; boolean_or) string_reverse &quot;G&quot; boolean_or integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 44
;;; -------------------------------------------------------
;;; Best plushy: (true close close string_concat string_take integer_+ &quot;&quot; integer_+ integer_= integer_+ string_includes? string_length &quot;&quot; 1 integer_+ in1 1 integer_+ string_reverse integer_+ true in1 true exec_dup in1 boolean_or close &quot;G&quot; integer_* integer_* integer_+)
;;; Best program: (true string_concat string_take integer_+ &quot;&quot; integer_+ integer_= integer_+ string_includes? string_length &quot;&quot; 1 integer_+ in1 1 integer_+ string_reverse integer_+ true in1 true exec_dup (in1 boolean_or) &quot;G&quot; integer_* integer_* integer_+)
;;; Best total error: 21
;;; Best errors: (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
;;; Best behaviors: (-1008 -736 -518 -348 -220 -128 -66 -28 -8 0 2 4 12 32 70 132 224 352 522 740 1012)
;;; 
;;; -------------------------------------------------------
;;;                Report for Generation 45
;;; -------------------------------------------------------
;;; Best plushy: (true close close close string_concat integer_+ integer_+ boolean_= integer_= integer_+ string_includes? integer_* 1 1 integer_+ in1 1 integer_+ string_reverse integer_+ true in1 true exec_dup in1 boolean_or close &quot;G&quot; integer_* integer_* &quot;C&quot; integer_+)
;;; Best program: (true string_concat integer_+ integer_+ boolean_= integer_= integer_+ string_includes? integer_* 1 1 integer_+ in1 1 integer_+ string_reverse integer_+ true in1 true exec_dup (in1 boolean_or) &quot;G&quot; integer_* integer_* &quot;C&quot; integer_+)
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
