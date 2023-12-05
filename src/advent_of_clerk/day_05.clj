;; # 🎄 Advent of Clerk: Day 5
(ns advent-of-clerk.day-05
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn parse-almanac
  [s]
  (let [[first & rest] (str/split-lines s)]
    [(map parse-long (-> first (str/split #": *") second (str/split #" +")))
     (->> rest
          (remove str/blank?)
          (reduce (fn [a s]
                    (if (re-find #" map:$" s)
                      (conj a [])
                      (update a (dec (count a))
                              conj (map parse-long (str/split s #" +")))))
                  []))]))

(parse-almanac example)

(defn map-val
  "Map a value to a new value using a list of almanac mappings"
  [mappings thing]
  (reduce (fn [t [minb mina num]]
            (if (<= mina t (+ mina num -1))
              (reduced (+ t (- minb mina)))
              t))
          thing
          mappings))

(mapv (partial map-val [[50 98 2]
                        [52 50 48]])
      [49 50 51 52 53])

(defn part1
  "Solve part 1"
  [s]
  (let [[seeds maps] (parse-almanac s)]
    (apply min
     (reduce (fn [nums m]
               (map (partial map-val m) nums))
             seeds
             maps))))

(part1 example)

#_(part1 (slurp "input_05.txt"))

(defn parse-almanac2
  "Almost like part 1, but seeds are ranges (vector of min and num)"
  [s]
  (-> s
      parse-almanac
      (update 0 #(partition 2 %))))

(parse-almanac2 example)

(defn map-range
  "Map a range ([min num]) using an almanac mapping,
   since the range could be split, it returns a 2-element
   vector of mapped ranges and unmapped ranges."
  [[dst-min src-min map-num] [range-min range-num]]
  (let [src-max (+ src-min map-num)
        range-max (+ range-min range-num)
        delta (- dst-min src-min)]
    (cond
      ;; range is below mapped range
      (<= range-max src-min)
      [[]
       [[range-min range-num]]]
      ;; range is above mapped range
      (>= range-min src-max)
      [[]
       [[range-min range-num]]]
      ;; range is entirely contained in mapped range
      (<= src-min range-min range-max src-max)
      [[[(+ range-min delta) range-num]]
       []]
      ;; range contains all of mapped range
      (< range-min src-min src-max range-max)
      [[[dst-min map-num]]
       [[range-min (- src-min range-min)]
        [src-max (- range-max src-max)]]]
      ;; range overlaps lower part of mapped range
      (and (< range-min src-min) (<= range-max src-max))
      [[[dst-min (- range-max src-min)]]
       [[range-min (- src-min range-min)]]]
      ;; range overlaps upper part of mapped range
      (and (<= src-min range-min) (< src-max range-max))
      [[[(+ range-min delta) (- src-max range-min)]]
       [[src-max (- range-max src-max)]]]
      :else ;; shouldn't get here
      (throw (Exception. (format "Shouldn't happen! R(%s:%s:%s) : S(%s:%s:%s)"
                                 range-min range-max range-num src-min src-max map-num))))))


(def test-mapping [100 10 10])
(mapv #(format "Range %s => %s" % (map-range test-mapping %))
      [[0 10]
       [5 10]
       [10 10]
       [15 10]
       [20 10]
       [5 20]])

(defn map-ranges
  "Map a list of ranges ([min num]) according to a list of almanac mappings"
  [mappings ranges]
  (apply into
         (reduce (fn [[mapped unmapped] mapping]
                   (let [r (map (partial map-range mapping) unmapped)]
                     [(reduce (partial into) mapped (map first r))
                      (reduce (partial into) [] (mapv second r))]))
                 [[] ranges]
                 mappings)))

(map-ranges [[50 98 2]
             [52 50 48]]
            [[79 14]
             [55 13]])

(defn part2
  "Solve part 2"
  [s]
  (let [[seeds maps] (parse-almanac2 s)]
    (->> maps
         (reduce (fn [ranges m]
                   (map-ranges m ranges))
                 seeds)
         (map first)
         (apply min))))

(part2 example)

#_(part2 (slurp "input_05.txt"))
