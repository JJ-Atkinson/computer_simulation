(ns computer-simulation.common
  (:require [fipp.edn :as pprint]
            [sc.api :as scc]))

(defn constrain-number-to [bit-count n]
  (bit-and (Math/floorMod (int n) (int (Math/pow 2 bit-count)))
           (int (- (Math/pow 2 bit-count) 1))))

(def max-16-bits (partial constrain-number-to 16))
(def max-8-bits (partial constrain-number-to 8))
(def max-4-bits (partial constrain-number-to 4))


(defn lowest-x-bits [x n] (bit-and n (int (- (Math/pow 2 x) 1))))

(defn upper-half-of-x-bit-n [x n]
  (bit-and (int (- (Math/pow 2 (int (/ x 2))) 1))
           (bit-shift-right n (int (/ x 2)))))