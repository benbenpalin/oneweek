(ns oneweek.day3)

(defn zig-zag-sequence
  "Returns an array of size n, derived from the original array, where the elements are arranged as follows:
  The first n/2 (rounded up) elements are in ascending order, and the last n/2 elements are in descending
  order. The resulting array is the lexicographically smallest possible zigzag sequence."
  [arr]
  (let [n (count arr)
        k (int (/ n 2))
        sorted (sort arr)
        first-k (take k sorted)
        last-k (reverse (drop k sorted))]
    (concat first-k last-k)))

(defn tower-breakers
  "Determines the winner of the game Tower Breaker, which is played as follows:
   - There are 2 players
   - Player 1 always moves first
   - Both players always play optimally
   - Initially there are n towers
   - Each tower is of height m
   - The players move in alternating turns
   - In each turn, a player can choose a tower of height x and reduce its height to y, where 1 < y < x  and y evenly divides x
   - If the current player is unable to make a move, they lose the game
   Given the values of n and m, returns which player will win."
  [n m]
  (cond
    (= m 1) 2
    (= n 1) 1
    (odd? n) 1
    :else 2))

(defn caesar-cipher-character
  "Shifts a character forward by a given integer. If the shift takes you past the end of the alphabet,
  just rotate back to the front of the alphabet. The character's case is preserved. Any characters in the
  input that are not letters (neither lowercase nor uppercase) are preserved as they are without any modification."
  [character k]
  (let [character-int (int character)]
    (if (and (not (<= 65 character-int 90))
             (not (<= 97 character-int 122)))
      character
      (let [case-shift (if (<= character-int 90)
                         65
                         97)]
        (-> character-int
            (- case-shift)
            (+ k)
            (mod 26)
            (+ case-shift)
            char)))))


(defn caesar-cipher-phrase
  "Applies the caesar cipher to a string of characters, returning a new string."
  [s k]
  (apply str
    (map #(caesar-cipher-character % k) s)))
