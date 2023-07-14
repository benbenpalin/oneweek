(ns oneweek.day2
  (:require [clojure.math.numeric-tower :as math]))

(defn lonelyinteger
  "Given an array of integers, where all elements but one occur twice, find the unique element."
  [a]
  (->> a
      frequencies
      (filter #(= 1 (second %)))
      ffirst))

(defn diagonalSum [arr coords]
  (->> coords
       (map get arr)
       (apply +)))

(defn diagonalDifference
  "Given a square matrix, calculate the absolute difference between the sums of its diagonals."
  [arr]
  (let [size (count arr)
        coords1 (range size)
        coords2 (reverse coords1)
        sum1 (diagonalSum arr coords1)
        sum2 (diagonalSum arr coords2)]
    (math/abs
      (- sum1 sum2))))

(defn countingSortReduce
  "Returns an array where each index corresponds to a unique value in the original array,
  and the value at each index represents the frequency of that value in the original array."
  [arr]
  (reduce
    #(update %1 %2 inc)
    (vec (take 100 (repeat 0)))
    arr))
