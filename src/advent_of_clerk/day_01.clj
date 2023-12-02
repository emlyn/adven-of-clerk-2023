;; # 🎄 Advent of Clerk: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(defn part1 [input]
  (->> input
       str/split-lines
       (map #(filter (set "0123456789") %))
       (map (juxt first last))
       (map #(parse-long (apply str %)))
       (reduce +)))

(part1 "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(part1 (slurp "input_01.txt"))

(def digits
  {"one" "1",
   "two" "2",
   "three" "3",
   "four" "4",
   "five" "5",
   "six" "6",
   "seven" "7",
   "eight" "8",
   "nine" "9"})

(defn rstr [s]
  (apply str (reverse s)))

(rstr "Hello")

(def numbers (re-pattern (clojure.string/join "|" (concat (keys digits) (vals digits) ["0"]))))
(def revnumbers (re-pattern (rstr (clojure.string/join "|" (concat (keys digits) (vals digits) ["0"])))))

(defn part2 [input]
  (->> input
       str/split-lines
       (map (fn [s] [(re-find numbers s)
                     (rstr (re-find revnumbers (rstr s)))]))
       (map (fn [s] (map #(digits % %) s)))
       (map #(Integer/parseInt (apply str %)))
       (reduce +)))

(part2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(part2 (slurp "input_01.txt"))
