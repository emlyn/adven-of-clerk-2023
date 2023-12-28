;; # 🎄 Advent of Clerk: Day 8
(ns advent-of-clerk.day-08
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
")

(defn parse-input
  [s]
  (let [[steps _ & nodes] (str/split-lines s)]
    [steps
     (->> nodes
          (map #(str/replace % #"\(|\)" ""))
          (map #(str/split % #" *= *|, *"))
          (map (fn [[a b c]] [a [b c]]))
          (into {}))]))

(parse-input example)

(defn part1
  "Solve part 1"
  [s]
  (let [[steps nodes] (parse-input s)]
    (loop [node "AAA"
           num 0
           [step & next-steps] (cycle steps)]
      (if (= node "ZZZ")
        num
        (recur (get-in nodes [node ({\L 0 \R 1} step)])
               (inc num)
               next-steps)))))

(part1 example)

#_(part1 (slurp "input_08.txt"))

(def example2 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn part2
  "Solve part 2 (only works on example input, fails on real input)"
  [s]
  (let [[steps nodemap] (parse-input s)]
    (loop [nodes (->> nodemap keys (filter #(= \A (last %))))
           num 0
           [step & next-steps] (cycle steps)]
      (if (every? #(= \Z (last %)) nodes)
        num
        (recur (map #(get-in nodemap [% ({\L 0 \R 1} step)]) nodes)
               (inc num)
               next-steps)))))

(part2 example2)

#_(part2 (slurp "input_08.txt")) ;; crashes on full data

(defn period
  "Find the repetition period of ending nodes."
  [steps nodemap node]
  (loop [node node
         num 0
         seen {}
         [step & next-steps] (cycle steps)]
    (if-let [start (seen node)]
      (let [diff (- num start)]
        (when (not= diff start)
          ;; Period is offset from zero, can't handle this
          (throw (Exception. (str "oops " start " != " diff))))
        diff)
      (recur (get-in nodemap [node ({\L 0 \R 1} step)])
             (inc num)
             (cond
               (not= \Z (last node))
               seen

               (empty? seen)
               (assoc seen node num)

               :else
               ;; Multiple end nodes in loop, can't handle this
               (throw (Exception. (str "oops " node seen))))
             next-steps))))

(defn gcd
  "Greatest common divisor"
  [a b]
  (cond
    (= 1 (min a b)) 1 ;; Not strictly necessary, but could speed it up
    (> a b) (recur (- a b) b)
    (< a b) (recur a (- b a))
    :else   a))

(defn lcm
  "Lowest common multiple"
  [a b]
  (* a (/ b (gcd a b))))

(defn part2b
  "Solve part 2"
  [s]
  (let [[steps nodemap] (parse-input s)]
    (->> nodemap
         keys
         (filter #(= \A (last %)))
         (map (partial period steps nodemap))
         (reduce lcm 1))))

(part2b example2)

#_(part2b (slurp "input_08.txt"))
