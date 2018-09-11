(ns computer-simulation.c8-bit
  (:require [fipp.edn :as pprint]
            [sc.api :as scc]
            [computer-simulation.common :as c]))

(def init-state 
  {:program-counter 0
   :reg-a 0
   :reg-b 0
   :instruction-counter 0
   :instruction-reg 0
   :ram-pointer 0
   :ram (map (fn [_] 0) (range 0 16))})

(def max-8 c/max-8-bits)
(def max-4 c/max-4-bits)
(def pointer-bits-half (partial c/lowest-x-bits 4))
(def instruction-code-half (partial c/upper-half-of-x-bit-n 8))

(def action-table
  "action table is a table of functions that accept 2 arguments.
   arg 1 is the state, arg 2 is the bus value (if present, not passed if not required).
   if it is an out command, the output is the bus value. If it is an input or reset 
   command, the return is the new state map.
   (bus values must be constrained to 8 bits)."
  (let [buffer-o (fn [location] (fn [s]   (get s location)))
        load-i   (fn [location] (fn [s b] (assoc s location b)))]
    {:ari #_:a-reg-in                  (load-i :reg-a)
     :aro #_:a-reg-out                 (buffer-o :reg-a)
     :bri #_:b-reg-in                  (load-i :reg-b)
     :bro #_:b-reg-out                 (buffer-o :reg-b)
     :pci #_:program-counter-in        (load-i :program-counter)
     :pco #_:program-counter-out       (buffer-o :program-counter)
     
     :ori #_:out-reg-in                (fn [s b] (println "The output is: " b) s)

     :iri #_:instruction-reg-in        (load-i :instruction-reg)
     :iro #_:instruction-reg-out       (fn [s] (pointer-bits-half (:instruction-reg s)))

     :rai #_:ram-in                    (fn [s b] (assoc s :ram (assoc (vec (:ram s)) (:ram-pointer s) b)))

     ; combined state-actions
     :rao #_:ram-out                   (fn [s] (get (vec (:ram s)) (:ram-pointer s)))
     :alo #_:alu-out                   (fn [s] (+ (:reg-a s) (:reg-b s))) ; no need to round since it goes to the buffer

     ;constrained to 4 bits
     :icr #_:instruction-counter-reset (fn [s b] (assoc s :instruction-counter 0))
     :ici #_:instruction-counter-in    (fn [s b] (assoc s :instruction-counter (max-4 b)))
     :ico #_:instruction-counter-out   (fn [s] (max-4 (:instruction-counter s)))

     :rpo #_:ram-pointer-out           (buffer-o :ram-pointer)
     :rpi #_:ram-pointer-in            (fn [s b] (assoc s :ram-pointer (max-4 b)))

     :cle #_:clock-enable              (fn [s b] (assoc s :program-counter (inc (:program-counter s))))
     :nop #_:no-op                     ;useful for placeholder a buffer-out
                                       (fn [s b] 0)

     :hlt #_:return-nop                (fn [s] 0)}))

(def instruction-table
  "assembly instructions. returns a map of lists of lists of actions that correspond to the action-table.
  Format: {:instruction-code [[micro-op1 micro-op2] [micro-op-3 micro-op-4]]}
  The first op must be a buffer-out, and all other ops may only be buffer-in"
  (let [fetch [[:pco :rpi] [:rao :iri :cle]]
        fetch& #(vec (concat fetch %1))]
    {2r0000 #_:add     (fetch& [[:alo :ari :icr]])
     2r0001 #_:out     (fetch& [[:aro :ori :icr]])
     2r0010 #_:load    (fetch& [[:iro :rpi] [:rao :ari :icr]])
     2r0011 #_:swap    (fetch& [[:iro :rpi] [:aro :rai] [:bro :ari] [:rao :bri :icr]])
     2r0100 #_:jmp     (fetch& [[:iro :pci :icr]])
     2r0101 #_:loadb   (fetch& [[:iro :rpi] [:rao :bri :icr]])
     2r0110 #_:addram  (fetch& [[:iro :rpi] [:rao :bri] [:alo :ari :icr]])
     ; first 2 must be nop, since that is the fetch cycle and the first executable step will be instruction 2 (#3)
     2r1111 #_:halt [[:hlt :nop] [:hlt :nop] [:hlt :nop]]}))

(def commands {:add     2r0000
               :out     2r0001
               :load    2r0010
               :swap    2r0011
               :jump    2r0100
               :loadb   2r0101
               :addram  2r0110
               :halt    2r1111})

(def pretty-commands (into {} (map #(vec (reverse %)) commands)))

(defn inc-instruction-counter [state]
  (assoc state :instruction-counter (inc (:instruction-counter state))))

(defn evaluate-action [state instruction-value position]
  (let [micro-op-steps  (get-in instruction-table [(instruction-code-half instruction-value) position])
        micro-op-functions (map #(get action-table %1) micro-op-steps)
        [bufo-instruction & bufi-instructions] micro-op-functions
        inc-state (inc-instruction-counter state)
        buffer-contents (max-8 (bufo-instruction state))]
    (if (= (first micro-op-steps) :hlt)
      :hlt
      (reduce (fn [a b] (b a buffer-contents) #_(%2 %1 buffer-contents))
                           ; we need to inc the counter here because this is how it works in the computer.
                           ; this lets the :icr (instruction-counter-reset) work when it is the last micro-op.
                           inc-state
                           bufi-instructions))))

(defn run-computer [init-state]
  (loop [state init-state
         counter 0]
    (let [instruction-counter-position (:instruction-counter state)
          current-instruction (:instruction-reg state)
          new-state (evaluate-action state current-instruction instruction-counter-position)]
      (if (and (not= new-state :hlt) (not= 500 counter))
        (recur new-state (inc counter))
        state))))

(defn compile-ram
  "Given a ram section, compile it to numerical storage for the computer.
  Elements with only one number will not be altered.
  Output is formal ram for the computer.
  Format: [[instruction#1 pointerbit#1] [instruction#2 pointerbit#2]]
  Examples: [[:out 0] [:halt 0]] [[:load 2r1111] [:loadb 2r1111] [:add 0] [:jump 2r0011]]"
  [code]
  (map (fn [command-item]
         (if (not= 1 (count command-item))
           (+ (bit-shift-left (get commands (first command-item)) 4) (second command-item))
           (first command-item)))
       code))

(def out-a-reg
  (merge init-state
         {:reg-a 210
          :ram   (-> (compile-ram [[:out 0] [:halt 0]])
                     (concat
                       (map (fn [_] 0) (range 0 14))))}))

(def fibonacci
  (assoc init-state :ram
                    (concat
                      (compile-ram [[:load 2r1111]
                                    [:loadb 2r1111]
                                    [:add   0]
                                    [:swap  2r1111]
                                    [:out   0]
                                    [:jump  2r0010]])
                      (map (fn [_] 1) (range 0 10)))))

(def swap-test
  (merge init-state
         {:reg-a 210
          :reg-b 115
          :ram   (-> (compile-ram [[:swap 2r0010] [:halt 0]])
                     (concat
                       (map (fn [_] 0) (range 0 14))))}))