;; # 🎄 Advent of Clerk: Day 17
(ns advent-of-clerk.day-17
  (:require [nextjournal.clerk]
            [emlyn.grid :as g]))

(def example "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(defn right
  "Rotate a direction clockwise"
  [[dx dy]]
  [(- dy) dx])

(defn left
  "Rotate a direction anti-clockwise"
  [[dx dy]]
  [dy (- dx)])

(defn plus-dir
  "Add a direction to a position"
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn update-cost
  "Update the cost of each cell in the grid by iterating one step ahead"
  [grid cost]
  (->> cost
       (mapcat (fn [[[pos dir num] cost]]
                 [[[pos dir num] cost]
                  (let [pos (plus-dir pos dir)]
                    (when-let [v (and (< num 3)
                                      (grid pos))]
                      [[pos dir (inc num)] (+ cost v)]))
                  (let [dir (right dir)
                        pos (plus-dir pos dir)]
                    (when-let [v (grid pos)]
                      [[pos dir 1] (+ cost v)]))
                  (let [dir (left dir)
                        pos (plus-dir pos dir)]
                    (when-let [v (grid pos)]
                      [[pos dir 1] (+ cost v)]))]))
       (remove nil?)
       (reduce (fn [m [k v]]
                 (update m k #(if % (min % v) v)))
               {})))

(let [grid (g/map-vals (comp parse-long str)
                       (g/grid example))]
  (update-cost grid {[[0 0] [1 0] 0] 0}))

(defn part1
  "Solve part 1."
  [s]
  (let [grid (g/map-vals (comp parse-long str)
                         (g/grid s))]
    (loop [cost {[[0 0] [1 0] 0] 0}]
      (let [nextcost (update-cost grid cost)]
        (if (= cost nextcost)
          (->> cost
               (filter (fn [[[pos _dir _num] _v]] (= pos [(dec (g/width grid)) (dec (g/height grid))])))
               (map (fn [[_k v]] v))
               (apply min))
          (recur nextcost))))))

(part1 example)

#_(part1 (slurp "input_17.txt"))

(defn update-cost2
  "Update the cost of each cell using the rules from part 2."
  [grid cost]
  (->> cost
       (mapcat (fn [[[pos dir num] cost]]
                 [[[pos dir num] cost]
                  (let [pos (plus-dir pos dir)]
                    (when-let [v (and (< num 10)
                                      (grid pos))]
                      [[pos dir (inc num)] (+ cost v)]))
                  (let [dir (right dir)
                        pos (plus-dir pos dir)]
                    (when-let [v (and (>= num 4)
                                      (grid pos))]
                      [[pos dir 1] (+ cost v)]))
                  (let [dir (left dir)
                        pos (plus-dir pos dir)]
                    (when-let [v (and (>= num 4)
                                      (grid pos))]
                      [[pos dir 1] (+ cost v)]))]))
       (remove nil?)
       (reduce (fn [m [k v]]
                 (update m k #(if % (min % v) v)))
               {})))

(defn part2
  "Solve part 2."
  [s]
  (let [grid (g/map-vals (comp parse-long str)
                         (g/grid s))]
    (loop [cost {[[0 0] [1 0] 0] 0}]
      (let [nextcost (update-cost2 grid cost)]
        (if (= cost nextcost)
          (->> cost
               (filter (fn [[[pos _dir num] _v]]
                         (and (= pos [(dec (g/width grid)) (dec (g/height grid))])
                              (>= num 4))))
               (map (fn [[_k v]] v))
               (apply min))
          (recur nextcost))))))

(part2 example)

(part2 "111111111111
999999999991
999999999991
999999999991
999999999991")

#_(part2 (slurp "input_17.txt"))
