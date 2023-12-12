;; # 🎄 Advent of Clerk: Day 12
(ns advent-of-clerk.day-12
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn parse-input
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #" +"))
       (map (fn [[row nums]]
              [row (map parse-long (str/split nums #", *"))]))))

(parse-input example)

(declare arrangements) ;; This will be the memoised version of arrangements*

(defn arrangements*
  "Calculate the number of possible arrangements of the groups given a row."
  [row [grp & more :as groups]]
  (cond
    ;; No more groups left, this is possible if there are no more # in the row:
    (nil? grp)
    (if (re-find #"#" row) 0 1)

    ;; Size of groups and gaps is bigger than remaining row, so impossible:
    (> (apply + (dec (count groups)) groups)
       (count row))
    0

    :else  ;; Find first possible position of first group:
    (if-let [[total prefix match _suffix]
             (re-find (re-pattern (format "^([^#]*?)([#?]{%d})([.?]|$)" grp)) row)]
      ;; Add number of arrangements of remaining groups in remaining row...
      (+ (arrangements (subs row (count total))
                       more)
         ;; ...to number of arrangements with first group later in the row...
         (if (not= \# (first match))
           (arrangements (subs row (inc (count prefix)))
                         groups)
           ;; ...unless first group cannot go later in the row.
           0))
      0)))

(def arrangements (memoize arrangements*))

(arrangements "???.###." [1 1 3]) ;; expect 1

(arrangements ".??..??...?##." [1 1 3]) ;; expect 4

(arrangements "?###????????.." [3 2 1]) ;; expect 10

(defn part1
  "Solve part 1."
  [s]
  (->> s
       parse-input
       (map (partial apply arrangements))
       (apply +)))

(part1 example)

#_(part1 (slurp "input_12.txt"))

(defn unfold
  "Unfold a row and list of groups"
  [times row groups]
  [(str/join \? (repeat times row))
   (take (* times (count groups)) (cycle groups))])

(unfold 5 ".#" [1])

(defn part2
  "Solve part 2 (requires a memoised arrangements fn for the full input)"
  [s]
  (->> s
       parse-input
       (map (partial apply unfold 5))
       (map (partial apply arrangements))
       (apply +)))

(part2 example)

#_(part2 (slurp "input_12.txt"))
