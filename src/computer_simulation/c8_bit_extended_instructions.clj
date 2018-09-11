(ns computer-simulation.c8-bit-extended-instructions
  (:require [computer-simulation.c8-bit :as base]))

(def init-state
  (merge base/init-state
         {:instruction-pointer 0
          :ram (map (fn [_] 0) (range 0 256))}))

(def action-table
  (merge base/action-table
         (let [buffer-o (fn [location] (fn [s]   (get s location)))
               load-i   (fn [location] (fn [s b] (assoc s location b)))]

           {:iro #_:instruction-reg-out       (buffer-o :instruction-reg)
            :ici #_:instruction-counter-in    (load-i :instruction-counter)
            :ico #_:instruction-counter-out   (buffer-o :instruction-counter)
            :ipi #_:instruction-pointer-in    (load-i :instruction-pointer)
            :ipo #_:instruction-pointer-out   (buffer-o :instruction-pointer)})))


(def instruction-table
  "assembly instructions. returns a map of lists of lists of actions that correspond to the action-table.
  Format: {:instruction-code [[micro-op1 micro-op2] [micro-op-3 micro-op-4]]}
  The first op must be a buffer-out, and all other ops may only be buffer-in"
  (let [fetch [[:pco :rpi] [:rao :iri :cle] [:pco :rpi] [:rao :ipi :cle]]
        fetch& #(vec (concat fetch %1))]
    {2r0000 #_:add     (fetch& [[:alo :ari :icr]])
     2r0001 #_:out     (fetch& [[:aro :ori :icr]])
     2r0010 #_:load    (fetch& [[:ipo :rpi] [:rao :ari :icr]])
     2r0011 #_:swap    (fetch& [[:ipo :rpi] [:aro :rai] [:bro :ari] [:rao :bri :icr]])
     2r0100 #_:jmp     (fetch& [[:ipo :pci :icr]])
     2r0101 #_:loadb   (fetch& [[:ipo :rpi] [:rao :bri :icr]])
     2r0110 #_:addram  (fetch& [[:ipo :rpi] [:rao :bri] [:alo :ari :icr]])
     ; first 2 must be nop, since that is the fetch cycle and the first executable step will be instruction 2 (#3)
     2r1111 #_:halt    [[:nop] [:nop] [:nop] [:nop] [:hlt :nop]]}))

(def commands {:add     2r0000
               :out     2r0001
               :load    2r0010
               :swap    2r0011
               :jump    2r0100
               :loadb   2r0101
               :addram  2r0110
               :halt    2r1111})

(def pretty-commands (into {} (map #(vec (reverse %)) commands)))

(defn evaluate-action [state]
  (let [instruction-value (:instruction-reg state)
        position (:instruction-counter state)
        micro-op-steps  (get-in instruction-table [instruction-value position])
        ;_ (println micro-op-steps "; " (pretty-commands (instruction-code-half instruction-value)))
        micro-op-functions (map #(get action-table %1) micro-op-steps)
        [bufo-instruction & bufi-instructions] micro-op-functions
        inc-state (base/inc-instruction-counter state)
        buffer-contents (base/max-8 (bufo-instruction state))]
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
    (let [;_ (println "\n===========\n")
          new-state (evaluate-action state)
          ;_ (println "Current-instruction" counter)
          ;_ (pprint/pprint new-state)
          ]
      (if (and (not= new-state :hlt) (not= 500 counter))
        (recur new-state (inc counter))
        state))))


(defn compile-ram
  "Given a ram section, compile it to numerical storage for the computer.
  Elements with only one number will not be altered.
  Output is formal ram for the computer.
  Format: [[instruction#1 pointerbit#1] [instruction#2 pointerbit#2]
  Examples: [[:out 0] [:halt 0]]; [[:load 2r1111] [:loadb 2r1111] [:add 0] [:jump 2r0011]]"
  [code]
  (mapcat (fn [command-item]
         (if (not= 1 (count command-item))  
           [(get commands (first command-item)) (second command-item)]
           [(first command-item)]))
       code))


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

