;; # 🎄 Advent of Clerk: Day 19
(ns advent-of-clerk.day-19
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

(defn parse-workflows
  [s]
  (->> s
       (map #(subs % 0 (dec (count %))))
       (map #(str/split % #"\{"))
       (map (fn [[name rules]]
              [name
               (->> (str/split rules #",")
                    (mapv #(if-let [[_ x op val target] (re-matches #"([xmas])(<|>)([0-9]+):([a-zA-Z]+)" %)]
                            [x op (parse-long val) target]
                            %)))]))
       (into {})))

(defn parse-parts
  [s]
  (->> s
       (map #(subs % 1 (dec (count %))))
       (map #(str/split % #", ?"))
       (map #(into {} (map (fn [s] (let [[k v] (str/split s #"=")]
                                     [k (parse-long v)])) %)))))

(defn parse-input
  [s]
  (let [[workflows _ parts] (->> s str/split-lines (partition-by str/blank?))]
    [(parse-workflows workflows)
     (parse-parts parts)]))

(parse-input example)

(defn apply-rule
  "Apply a single rule to a part, returning the new state."
  [part rule]
  (if (string? rule)
    rule
    (let [[x op val target] rule
          partval (part x)]
      (case op
        ">" (when (> partval val) target)
        "<" (when (< partval val) target)
        (throw (Exception. (str "Unknown op: " op)))))))

(defn apply-workflow
  "Apply a workflow of rules to a part, returning the state after the selected rule."
  [workflow part]
  (->> workflow
       (map (partial apply-rule part))
       (remove nil?)
       first))

(defn accept?
  "Determine whether a part is accepted by a set of workflows."
  [workflows part]
  (loop [state "in"]
    (cond
      (= state "A") true
      (= state "R") false
      :else
      (if-let [workflow (workflows state)]
        (recur (apply-workflow workflow part))
        (throw (Exception. (str "Unknown state: " state)))))))

(defn part1
  "Solve part 1."
  [s]
  (let [[workflows parts] (parse-input s)]
    (->> parts
         (filter (partial accept? workflows))
         (mapcat vals)
         (apply +))))


(part1 example)

#_(part1 (slurp "input_19.txt"))

(defn update-range*
  "Update a range (min-max pair) given a rule."
  [[vmin vmax] op val reversed?]
  (cond
    (and (= op "<") (not reversed?)) [vmin (min vmax (dec val))]
    (and (= op ">") (not reversed?)) [(max vmin (inc val)) vmax]
    (and (= op "<") reversed?) [(max vmin val) vmax]
    (and (= op ">") reversed?) [vmin (min vmax val)]
    :else (throw (Exception. (str "Unknown op: " op)))))

(update-range* [100 200] "<" 150 false)
(update-range* [100 200] "<" 150 true)

(defn update-range
  "Update the appropriate category in a range map (map of category to range) given a rule."
  [range [x op val _target] reversed?]
  (update range x update-range* op val reversed?))

(update-range {"x" [1 4000] "m" [1 4000]}
              ["x" "<" 1000 "m"]
              false)
(update-range {"x" [1 4000] "m" [1 4000]}
              ["x" "<" 1000 "m"]
              true)

(defn update-ranges
  "Update a range map given its current workflow name and the set of workflows."
  [workflows [wf ranges]]
  (let [workflow (workflows wf)]
    (if workflow
      (first
       (reduce (fn [[acc range] rule]
                 (if (string? rule)
                   (reduced [(conj acc [rule range]) nil])
                   [(conj acc [(last rule) (update-range range rule false)])
                    (update-range range rule true)]))
               [[] ranges]
               workflow))
      [[wf ranges]])))

(def test-workflows
  {"in" [["x" ">" 1000 "b"] ["m" ">" 1000 "c"] "d"]
   "b"  [["m" ">" 2000 "A"] ["x" "<" 2500 "d"] "R"]
   "c"  [["x" "<" 3000 "R"] "A"]
   "d"  [["m" ">" 3000 "A"] "R"]})

(def test-ranges
  ["in" {"x" [1 4000] "m" [1 4000]}])

(defn iterate-test [data]
  (mapcat (partial update-ranges test-workflows) data))

(iterate-test [test-ranges])
(iterate-test (iterate-test [test-ranges]))

(defn part2
  "Solve part 2."
  [s]
  (let [[workflows] (parse-input s)
        result
        (loop [ranges [["in" {"x" [1 4000], "m" [1 4000], "a" [1 4000], "s" [1 4000]}]]]
          (let [next-ranges (into [] (mapcat (partial update-ranges workflows) ranges))]
            (if (not= ranges next-ranges)
              (recur next-ranges)
              ranges)))]
    (->> result
         (filter (comp #{"A"} first))
         (map second)
         (map vals)
         (map #(map (comp inc - (partial apply -)) %))
         (map (partial apply *))
         (apply +))))

(part2 example)

#_(part2 (slurp "input_19.txt"))
