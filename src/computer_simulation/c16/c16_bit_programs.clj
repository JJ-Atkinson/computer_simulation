(ns computer-simulation.c16.c16-bit-programs
  (require [computer-simulation.c16.c16-bit-with-logic :as c16
            :refer [compile-ram
                    init-state
                    control-values]]))

(def out-a-reg
  (merge init-state
         {:reg-a 210
          :ram   (-> (compile-ram [[:out 0] [:halt 0]])
                     (concat
                       (map (fn [_] 0) (range 0 14))))}))

(def fibonacci
  (assoc init-state :ram
                    (concat
                      (compile-ram [[:load 0x0F]
                                    [:loadb 0x0F]
                                    [:add 0]
                                    [:swap 0x0F]
                                    [:out 0]
                                    [:jump 0x02]])
                      (map (fn [_] 1) (range 0 10)))))

(def swap-test
  (merge init-state
         {:reg-a 210
          :reg-b 115
          :ram   (-> (compile-ram [[:swap 2r0010] [:halt 0]])
                     (concat
                       (map (fn [_] 0) (range 0 14))))}))

(def if-test
  (merge init-state
         {:reg-a 100
          :reg-b 100
          :ram   (concat
                   (compile-ram [[      :loadr0i   :else]
                                 [      :if        (:> control-values)]
                                 [      :loadai    1]
                                 [      :jump      :end]
                                 [:else :loadai    0]
                                 [:end  :out       0]
                                 [      :halt      0]])
                   [])}))

(def count-test
  (merge init-state
         {:reg-a 0
          :reg-b 50
          :ram (compile-ram [[        :loadr0i :end]
                             [:while  :if      (:< control-values)]
                             [        :loadbi  1]
                             [        :add     0]
                             [        :out     0]
                             [        :loadbi  5]
                             [        :jump    :while]
                             [:end    :halt    0]])}))

(def is-prime
  (merge init-state
         {:reg-a 7
          :reg-b 2
          ;                   :label            :command/var-val   :pointer
          :ram (compile-ram [[                  :loadr0i           :loop-end]
                             [                  :loadbi            2]
                             [                  :if                (:> control-values)]
                             [:loop-start       :loadr0i           :loop-end]
                             [                  :if                (:> control-values)]
                             [                  :stoa              :ram-spot-a]
                             [                  :modulo            0]
                             [                  :stob              :ram-spot-b]
                             [                  :loadbi            0]
                             [                  :loadr0i           :continue]
                             [                  :if                (:= control-values)]
                             [:not-prime        :loadai            ]
                             [                  :out               0]
                             [                  :halt              0]
                             [:continue         :loadb             :ram-spot-b]
                             [                  :loadai            1]
                             [                  :add               0]
                             [                  :swap              :ram-swap-space]
                             [                  :loada             :ram-spot-a]
                             [                  :jump              :loop-start]
                             [:loop-end         :loadai            1]
                             [                  :out               0]
                             [                  :halt              0]
                             [:ram-spot-a       0x00]
                             [:ram-spot-b       0x00]
                             [:ram-swap-space   0x00]])}))


;(defn full-ram-tester
;  (merge init-state
;         {:ram (-> (compile-ram [[:ram-pos     0]
;                                 [             :loadbi  [0]]
;                                 [             :stob    :ram-pos]
;                                 
;                                 [:loop-start  :loada   :ram-pos]
;                                 [             :loadr0i :end]
;                                 [             :loadbi  50]
;                                 [             :ifx     (:= control-values)]
;                                 [             :loadbi  :ram-pos]
;                                 [             :stob    :ram-pos]
;                                 [             :loadai  1]
;                                 [             :add     0]
;                                 
;                                 [             :jump    :loop-start]
;                                 [:end         :halt    0]])
;                   (c16/paddout-ram 0 50))}))