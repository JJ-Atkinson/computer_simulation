(ns computer-simulation.c16.c16-bit-debugging
  (require [computer-simulation.c16.c16-bit-with-logic :as c16
            :refer [commands
                    instruction-table]]
           [clojure.string :as s]
           [colorize.core :as co]
           [sc.api :as scc]))


(def pretty-commands (into {} (map #(vec (reverse %)) commands)))


(defn print-computed-info
  "Very high tech and innovative way to show the internals of the computer."
  [state]
  (if (= state :hlt)
    (println (co/color :inverse "Halt"))
    
    (letfn [(highlight-in-vec [pos ve color]
              (let [last-half (drop 1 (second (split-at pos ve)))
                    x (get ve pos)
                    first-half (first (split-at pos ve))]
                (str "[" (s/join " " first-half) " " (co/color color x) " " (s/join " " last-half) "]")))]
      (println (co/color :red-bg "    "))
      (println (co/color :green-bg ":Program-counter\t") (co/color :green (:program-counter state)))
      (println (co/color :green-bg "Clocked-instruction\t") (co/color :green (get pretty-commands
                                                             (get (vec (:ram state)) (:program-counter state))
                                                             :invalid-instruction)))
      (println (co/color :green-bg ":Instruction-reg\t") (co/color :green (get pretty-commands (:instruction-reg state))))
      (println (co/color :yellow-bg ":Instruction-counter\t") (co/color :yellow (:instruction-counter state)))
      (println "Ops-in-instruction\t" (highlight-in-vec (:instruction-counter state)
                                                        (vec (get instruction-table (:instruction-reg state)))
                                                        :yellow))
      (println (co/color :cyan-bg ":Instruction-pointer\t") (co/color :cyan (:instruction-pointer state)))
      (let [pos (:ram-pointer state)
            ram (vec (:ram state))]
        (println ":Ram-pointer\t" (co/color :blue pos))
        (println ":Ram\t" (highlight-in-vec pos ram :blue)))
      (doall (map (fn [[s k]] (println (co/color :red-bg s) (co/color :red (get state k))) 0)
                  [[":Reg-a\t" :reg-a]
                   [":Reg-b\t" :reg-b]
                   [":Reg-0\t" :reg-0]]))
      nil)))


(defn rel-data-in-eval-action [spy-data] (dissoc (:sc.ep/local-bindings spy-data) 'micro-op-functions 'bufi-instructions 'bufo-instruction))

(defn print-states [states-list]
  (reduce (fn [counter state]
            (println "\n==========================")
            (println (co/color :bold counter) "\n")
            (print-computed-info state)
            (inc counter))
          1
          states-list)
  nil)

