(ns oneweek.day1
  (:require
    [clojure.string :as str]))

(defn plusMinus [arr]
  "Given an array of integers, calculate the ratios of its elements that are positive,
   negative, and zero. Print the decimal value of each fraction on a new line with
   places after the decimal."
  (let[rat (reduce
               #(cond
                  (pos? %2) (update %1 0 inc)
                  (neg? %2) (update %1 1 inc)
                  (zero? %2) (update %1 2 inc))
               [0 0 0]
               arr)]
    (doseq [i rat] (println (format "%.6f" (float (/ i (count arr))))))))

(defn miniMaxSum [arr]
  "Given five positive integers, find the minimum and maximum values that can be calculated
  by summing exactly four of the five integers. Then print the respective minimum and maximum
  values as a single line of two space-separated long integers."
  (let [indices (range 5)
        combos (map #(assoc arr % 0) indices)
        sums (map #(apply + %) combos)]
    (println (apply min sums) (apply max sums))))

(defn twelve-as-zero [hour]
  (if (= "12" hour)
    "00"
    hour))

(defn timeConversion [s]
  "Given a time in -hour AM/PM format, convert it to military (24-hour) time."
  (let [hour (-> s
                 (subs 0 2)
                 twelve-as-zero)
        min-sec (subs s 2 8)
        am? (= "AM" (subs s 8 10))
        military-hour (if am?
                        hour
                        (-> hour
                            Integer/parseInt
                            (+ 12)))]
    (str military-hour min-sec)))



