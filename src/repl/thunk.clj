(ns repl.thunk
  (require computer-simulation.c16.c16-bit-with-logic
           computer-simulation.c16.c16-bit-debugging
           computer-simulation.c16.c16-bit-programs
           computer-simulation.c8-bit-extended-instructions
           computer-simulation.c8-bit
           computer-simulation.common
           clojure.tools.namespace.repl
           colorize.core
           sc.api))

(defn requires []
  (require '[clojure.tools.namespace.repl :as r]
           '[computer-simulation.c16.c16-bit-with-logic :as c16]
           '[computer-simulation.c16.c16-bit-programs :as c16-pgrm]
           '[computer-simulation.c16.c16-bit-debugging :as c16-dbg]
           '[computer-simulation.c8-bit :as c8]
           '[computer-simulation.c8-bit-extended-instructions :as c8-ext]
           '[computer-simulation.common :as common]
           '[sc.api :as scc]))

(defn refresh []
  (let [refresh-res (clojure.tools.namespace.repl/refresh)]
    (requires)
    refresh-res))
