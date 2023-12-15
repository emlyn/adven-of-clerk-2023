;; # 🎄 Advent of Clerk: Day 9
(ns advent-of-clerk.day-09
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn parse-input
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #" +"))
       (map (partial map parse-long))))

(parse-input example)

(defn next-prediction
  "Predict the next value in a sequence"
  [s]
  (if (apply = 0 s)
    0
    (+ (last s)
       (next-prediction (map - (rest s) s)))))

(defn part1
  "Solve part 1"
  [s]
  (->> s
       parse-input
       (map next-prediction)
       (apply +)))

(part1 example)

#_(part1 (slurp "input_09.txt"))

(defn part2
  "Solve part 2"
  [s]
  (->> s
       parse-input
       (map reverse)
       (map next-prediction)
       (apply +)))

(part2 example)

#_(part2 (slurp "input_09.txt"))
