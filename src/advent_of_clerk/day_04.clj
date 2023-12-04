;; # 🎄 Advent of Clerk: Day 4
(ns advent-of-clerk.day-04
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn parse-card
  "Parse a line into a representation of a card"
  [s]
  (let [[a b] (str/split s #": *")
        [_ id] (str/split a #" +")
        [win have] (str/split b #" *\| *")]
    [id
     (map parse-long (str/split win #" +"))
     (map parse-long (str/split have #" +"))]))

(parse-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn score
  "The score of a card"
  [[_id win have]]
  (let [win (into #{} win)]
    (reduce (fn [score n]
              (if (win n)
                (if (zero? score) 1 (* score 2))
                score))
            0
            have)))

(defn part1
  "Solve part 1"
  [s]
  (->> s
       str/split-lines
       (map parse-card)
       (map score)
       (apply +)))

(part1 example)

(part1 (slurp "input_04.txt"))

(def example2 "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn score2
  "Count of numbers that are in the winning set"
  [[_id win have]]
  (let [win (into #{} win)]
    (count (filter win have))))

(score2 (parse-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))

(defn part2
  "Solve part 2"
  [s]
  (let [initial (->> s
                     str/split-lines
                     (map parse-card))]
    (loop [[card & rest] initial
           ncards (into {} (map #(vector (first %) 1) initial))]
      (if card
        (let [nwin (score2 card)]
          (recur rest
                 (reduce (fn [nc [n]]
                           (update nc n #(+ % (ncards (first card)))))
                         ncards
                         (take nwin rest))))
        (->> ncards
             vals
             (apply +))))))

(part2 example2)

(part2 (slurp "input_04.txt"))
