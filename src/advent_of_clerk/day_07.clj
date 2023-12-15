;; # 🎄 Advent of Clerk: Day 7
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(defn parse-cards
  [s]
  (->> s
       str/split-lines
       (map #(let [[cards bid] (str/split % #" +")]
               [cards (parse-long bid)]))))

(parse-cards example)

(def card-scores
  (into {}
        (map-indexed #(vector %2 (+ 2 %1)) "23456789TJQKA")))

(def high_card 1)
(def one_pair 2)
(def two_pair 3)
(def three_of_a_kind 4)
(def full_house 5)
(def four_of_a_kind 6)
(def five_of_a_kind 7)

(def type-scores
  {[1 1 1 1 1] high_card
   [1 1 1 2]   one_pair
   [1 2 2]     two_pair
   [1 1 3]     three_of_a_kind
   [2 3]       full_house
   [1 4]       four_of_a_kind
   [5]         five_of_a_kind})

(defn hand-type
  [hand]
  (->> hand
       frequencies
       vals
       sort
       type-scores))

(mapv hand-type
      (map first (parse-cards example)))

(defn part1
  [s]
  (->> s
       parse-cards
       (sort-by (fn [[hand _bid]]
                  (into [(hand-type hand)] (map card-scores hand))))
       (map (fn [rank [_hand bid]] (* rank bid)) (iterate inc 1))
       (apply +)))

(part1 example)

#_(part1 (slurp "input_07.txt"))

(def card-scores2
  (into {}
        (map-indexed #(vector %2 (+ 1 %1)) "J23456789TQKA")))

(defn add-jokers
  [jokers counts]
  (if (empty? counts)
    [jokers]
    (update (vec counts) (dec (count counts)) + jokers)))

(defn hand-type2
  [hand]
  (->> hand
       (remove #{\J})
       frequencies
       vals
       sort
       (add-jokers (count (filter #{\J} hand)))
       type-scores))

(hand-type2 "KTJJT")

(defn part2
  [s]
  (->> s
       parse-cards
       (sort-by (fn [[hand _bid]]
                  (into [(hand-type2 hand)] (map card-scores2 hand))))
       (map (fn [rank [_hand bid]] (* rank bid)) (iterate inc 1))
       (apply +)))

(part2 example)

#_(part2 (slurp "input_07.txt"))
