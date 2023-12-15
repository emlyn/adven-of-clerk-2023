;; # 🎄 Advent of Clerk: Day 13
(ns advent-of-clerk.day-13
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn parse-input
  [s]
  (->> s
       str/split-lines
       (partition-by str/blank?)
       (remove (comp str/blank? first))))

(parse-input example)

(defn transpose
  "Transpose rows<->cols in a vector of strings"
  [pattern]
  (apply map str pattern))

(transpose
 ["1234"
  "abcd"
  "6789"
  "wxyz"])

(defn reflections
  "Between which rows can you reflect the pattern leaving it unchanged?
   (for columns we'll just transpose then look for rows)."
  [pattern]
  (for [r (range 1 (count pattern))
        :let [[top bot] (split-at r pattern)]
        :when (apply = true (map = (reverse top) bot))]
    r))

(reflections (first (parse-input example)))
(reflections (transpose (first (parse-input example))))
(reflections (second (parse-input example)))
(reflections (transpose (second (parse-input example))))

(defn part1
  "Solve part 1."
  [s]
  (let [patterns (parse-input s)]
    (+ (* 100 (apply + (mapcat reflections patterns)))
       (apply + (mapcat (comp reflections transpose) patterns)))))

(part1 example)

#_(part1 (slurp "input_13.txt"))

(defn smudged-reflections
  "Look for near-reflections where there are `smudges` mismatched elements."
  [smudges pattern]
  (for [r (range 1 (count pattern))
        :let [[top bot] (split-at r pattern)
              diff (apply +
                          (map (fn [a b]
                                 (count (remove true? (map = a b))))
                               (reverse top)
                               bot))]
        :when (= diff smudges)]
    r))

(defn part2
  [s]
  (let [patterns (parse-input s)]
    (+ (* 100 (apply + (mapcat (partial smudged-reflections 1) patterns)))
       (apply + (mapcat (comp (partial smudged-reflections 1) transpose) patterns)))))

(part2 example)

#_(part2 (slurp "input_13.txt"))
