(ns computer-simulation.c16.c16-bit-with-logic
  (:require [sc.api :as scc]
            [computer-simulation.common :as c])
  (:import (java.util Collections)))

(def init-state
  {:program-counter     0
   :reg-a               0
   :reg-b               0
   :reg-0               0
   :instruction-counter 0
   :instruction-reg     0
   :instruction-pointer 0
   :ram-pointer         0
   :ram                 (map (fn [_] 0) (range 0 16))})

(def max-16 c/max-16-bits)

(def action-table
  "action table is a table of functions that accept 2 arguments.
   arg 1 is the state, arg 2 is the bus value (if present, not passed if not required).
   if it is an out command, the output is the bus value. If it is an input or reset 
   command, the return is the new state map.
   (bus values must be constrained to 8 bits)."
  (letfn [(buffer-o [location] (fn [s] (get s location)))
          (load-i [location] (fn [s b] (assoc s location b)))]
    {:ari #_:a-reg-in                  (load-i :reg-a)
     :aro #_:a-reg-out                 (buffer-o :reg-a)
     :bri #_:b-reg-in                  (load-i :reg-b)
     :bro #_:b-reg-out                 (buffer-o :reg-b)
     :pci #_:program-counter-in        (load-i :program-counter)
     :pco #_:program-counter-out       (buffer-o :program-counter)
     :0ri #_:0-reg-in                  (load-i :reg-0)
     :0ro #_:0-reg-out                 (buffer-o :reg-0)
     :ici #_:instruction-counter-in    (load-i :instruction-counter)
     :ico #_:instruction-counter-out   (buffer-o :instruction-counter)
     :iri #_:instruction-reg-in        (load-i :instruction-reg)
     :iro #_:instruction-reg-out       (buffer-o :instruction-reg)
     :ipi #_:instruction-pointer-in    (load-i :instruction-pointer)
     :ipo #_:instruction-pointer-out   (buffer-o :instruction-pointer)

     :ori #_:out-reg-in                (fn [s b] (println "The output is: " b) s)

     :rai #_:ram-in                    (fn [s b] (assoc s :ram (assoc (vec (:ram s)) (:ram-pointer s) b)))

     ; combined state-actions
     :rao  #_:ram-out                  (fn [s] (get (vec (:ram s)) (:ram-pointer s)))
     :alo  #_:alu-out                  (fn [s] (+ (:reg-a s) (:reg-b s))) ; no need to round since it goes to the buffer
     :modo #_:mod-out                  (fn [s] (mod (:reg-a s) (:reg-b s)))
     
     ; specialized commands

     ; alu
     ; comes with a<b, b<a, a=b. When the condition is true, :instruction-counter-out is activated. Otherwise :reg-0-out
     ; is activated (i.e. same position). This should be paired with :instruction-counter-in as a conditional jump.
     ; This is implemented with a single function that uses the :instruction-pointer top 2 bits.
     ; Highest 2 bits are a number for selecting [0 <] [1 =] [2 >].
     ; This is a :out function.
     :ifx #_:if-condition              (fn [s] (let [instruction-pointer (:instruction-pointer s)
                                                     bits-value (bit-and 2r11 (bit-shift-right instruction-pointer 14))
                                                     conditional (get [< = >] bits-value)
                                                     reg-a (:reg-a s)
                                                     reg-b (:reg-b s)]
                                                 (if (conditional reg-a reg-b)
                                                   (:program-counter s)
                                                   (:reg-0 s))))


     :icr #_:instruction-counter-reset (fn [s b] (assoc s :instruction-counter 0))

     :rpo #_:ram-pointer-out           (buffer-o :ram-pointer)
     :rpi #_:ram-pointer-in            (load-i :ram-pointer)

     :cle #_:clock-enable              (fn [s b] (assoc s :program-counter (inc (:program-counter s))))
     :2cl #_:2-clock-enable            (fn [s b] (assoc s :program-counter (+ 2 (:program-counter s))))

     ;useful for placeholder a buffer-out
     :nop #_:no-op                     (fn [s b] 0)

     :hlt #_:return-nop                (fn [s] 0)}))

(def instruction-table
  "assembly instructions. returns a map of lists of lists of actions that correspond to the action-table.
  Format: {:instruction-code [[micro-op1 micro-op2] [micro-op-3 micro-op-4]]}
  The first op must be a buffer-out, and all other ops may only be buffer-in"
  (let [fetch [[:pco :rpi] [:rao :iri :cle] [:pco :rpi] [:rao :ipi :cle]]
        fetch& #(vec (concat fetch %1))]
    {0x00 #_:add     (fetch& [[:alo :ari :icr]])
     0x01 #_:out     (fetch& [[:aro :ori :icr]])
     0x02 #_:loada   (fetch& [[:ipo :rpi] [:rao :ari :icr]])
     0x03 #_:loadai  (fetch& [[:ipo :ari :icr]])
     0x04 #_:swap    (fetch& [[:ipo :rpi] [:aro :rai] [:bro :ari] [:rao :bri :icr]])
     0x05 #_:jump    (fetch& [[:ipo :pci :icr]])
     0x06 #_:loadb   (fetch& [[:ipo :rpi] [:rao :bri :icr]])
     0x07 #_:loadbi  (fetch& [[:ipo :bri :icr]])
     0x08 #_:addram  (fetch& [[:ipo :rpi] [:rao :bri] [:alo :ari :icr]])
     0x09 #_:if      (fetch& [[:ifx :pci :icr]])
     0x0a #_:loadr0  (fetch& [[:ipo :rpi] [:rao :0ri :icr]])
     0x0b #_:loadr0i (fetch& [[:ipo :0ri :icr]])


     ; first 4 must be nop, since that is the fetch cycle and the first executable step will be instruction 4 (#5)
     0x0F #_:halt    (vec (repeat 5 [:hlt :nop]))
     0x10 #_:stoa    (fetch& [[:ipo :rpi] [:aro :rai :icr]])
     0x11 #_:stob    (fetch& [[:ipo :rpi] [:bro :rai :icr]])
     0x12 #_:stor0   (fetch& [[:ipo :rpi] [:0ro :rai :icr]])
     0x13 #_:modulo  (fetch& [[:modo :ari :icr]])}))

(def commands {:add     0x00
               :out     0x01
               :loada   0x02
               :loadai  0x03
               :swap    0x04
               :jump    0x05
               :loadb   0x06
               :loadbi  0x07
               :addram  0x08
               :if      0x09
               :loadr0  0x0a
               :loadr0i 0x0b
               :halt    0x0F
               :stoa    0x10
               :stob    0x11
               :stor0   0x12
               :modulo  0x13})

(def control-values
  "Control values for :instruction-pointer for special commands, like :if."
  {:< (bit-shift-left 0 14)
   := (bit-shift-left 1 14)
   :> (bit-shift-left 2 14)})

(defn inc-instruction-counter [state]
  (assoc state :instruction-counter (inc (:instruction-counter state))))

(defn evaluate-clock-tick [state]
  (let [instruction-value (:instruction-reg state)
        position (:instruction-counter state)
        micro-op-steps (get-in instruction-table [instruction-value position])
        micro-op-functions (map #(get action-table %1) micro-op-steps)
        [bufo-instruction & bufi-instructions] micro-op-functions
        inc-state (inc-instruction-counter state)
        buffer-contents (max-16 (bufo-instruction state))]
    (if (= (first micro-op-steps) :hlt)
      :hlt
      (reduce #(%2 %1 buffer-contents)
              ; we need to inc the counter here because this is how it works in the computer.
              ; this lets the :icr (instruction-counter-reset) work when it is the last micro-op.
              inc-state
              bufi-instructions))))

(defn run-computer
  ([init-state]
   (run-computer init-state false -1))

  ([init-state capture-intermediate-states?]
   (run-computer init-state capture-intermediate-states? -1))

  ([init-state capture-intermediate-states? max-clock-count]
   (loop [state init-state
          counter 0
          accumulator []]
     (let [new-state (try (evaluate-clock-tick state)
                          (catch Exception e
                            (println e)
                            (flush)
                            :hlt))
           new-accumulator (if capture-intermediate-states?
                             (conj accumulator new-state)
                             new-state)]
       (if (and (not= new-state :hlt)
                (not= max-clock-count counter))
         (recur new-state
                (inc counter)
                new-accumulator)
         new-accumulator)))))




(defn compile-ram
  "Given a code section, compile it to numerical storage for the computer.
  The format is simple, since each command for the computer must have an
  instruction and instruction pointer. The basic form is a tuple of a 
  keyword command from `commands` and its matching pointer bit. E.g. 
  [:loadai 0x0F]. Labels can be used to point to other commands. The format 
  for their use is to include a keyword at the beginning of the command. E.g. 
  [:loop-1-entrance :loadai 0x0F]. This will be removed and any other occurrences
  of `:loop-1-entrance` will be replaced with the position in ram that the 
  command is. E.g. `[:jump :loop-1-entrance]` will be replaced with `[:jump 0x0C]`,
  assuming :loop-1-entrance is at ram position `0x0C`. Pre-loaded ram locations
  can also be named. They follow this format: `[:variable 0x00]`. The tag must not
  collide with any existing assembly commands. Positions are relative to
  `starting-block`. Finally, any pointers wrapped in [] will have the total length
  of this ram block to the pointer. Useful for referencing ram that comes after
  the code block. This only accepts integers, not labels.
  
  Output is formal ram for the computer.
  Examples: [[:out 0] [:halt 0]]; [[:load 2r1111] [:loadb 2r1111] [:add 0] [:jump 2r0011]]
            [[:start :loada 0xFF] [:swap 0] [:add 0xFE] [:jump :start]]
            
  Acceptable elements:
  [:label 0]          Labeled Variable
  [0]                 Variable
  [:label :command 0] Labeled Command
  [:command 0]        Command
  "
  ([code]
   (compile-ram code 0))

  ([code starting-block]
   (compile-ram code starting-block false))

  ([code starting-block basic-check?]
   (let [classify (fn [line]
                    (case (map keyword? line)
                      [true false] (if (get commands (first line))
                                     :command
                                     :labeled-var)
                      [true true] :command
                      [false] :var
                      [true true false] :labeled-command
                      [true true true] :labeled-command))

         [ending-block labels] (reduce
                                 (fn [[curr-pos labels] line]
                                   (case (classify line)
                                     :command [(+ 2 curr-pos) labels]
                                     :labeled-command [(+ 2 curr-pos) (assoc labels (first line) curr-pos)]
                                     :var [(inc curr-pos) labels]
                                     :labeled-var [(inc curr-pos) (assoc labels (first line) curr-pos)]))
                                 [starting-block {}]
                                 code)

         ; helpful warning to users : since labeled vars are simply fallthrough command syntax,
         ; typos in the code may be interpreted as labeled vars. A simple way to remove 
         ; grief from their lives is to examine whether or not labeled vars are sandwiched
         ; between other bits of code. 
         _ (when basic-check?
             (let [structure (map classify code)
                   possible-error-positions?
                   (filter #(< -1 %) (map (fn [code] (Collections/indexOfSubList structure code))
                                          [[:labeled-command :labeled-var :labeled-command]
                                           [:command :labeled-var :labeled-command]
                                           [:labeled-command :labeled-var :command]
                                           [:command :labeled-var :command]]))
                   possible-errors (map (fn [pos] (take 3 (drop pos code)))
                                        possible-error-positions?)]

               (when (not-empty possible-errors) (println "Possible typo in the code."
                                                          possible-errors))))
         
         de-labeled (reduce (fn [de-labeled line]
                              (conj de-labeled
                                    (case (classify line)
                                      :labeled-command (drop 1 line)
                                      :labeled-var (drop 1 line)
                                      line)))
                            []
                            code)

         compile-pointer (fn [p] (if (vector? p)
                                   (+ ending-block (first p))
                                   p))]

     (reduce (fn [compiled line]
               (concat compiled
                       (case (classify line)
                         :command [(get commands (first line))
                                   (get labels (second line) (compile-pointer (second line)))]
                         :var [(compile-pointer (first line))])))
             []
             de-labeled))))

(defn paddout-ram [ram filler width]
  (concat ram (repeat filler (- width (count ram)))))