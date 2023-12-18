;; # 🎄 Advent of Clerk: Day 17
(ns advent-of-clerk.day-17
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

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

(defn ->2darray
  "Convert string to 2D array (vector of vector of chars)"
  [s]
  (mapv vec (str/split-lines s)))

(defn cell
  "Get cell value from 2D array"
  [grid [x y]]
  (get-in grid [y x]))

(defn set-cell [grid [x y] v]
  (assoc-in grid [y x] v))

(defn map-grid
  "Map a function over a 2D array"
  [f grid]
  (reduce (fn [g [x y]]
            (set-cell g [x y] (f (cell grid [x y]) x y)))
          grid
          (for [x (range (count (first grid)))
                y (range (count grid))]
            [x y])))

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
                    (when-let [v (and (< num 4)
                                      (cell grid pos))]
                      [[pos dir (inc num)] (+ cost v)]))
                  (let [dir (right dir)
                        pos (plus-dir pos dir)]
                    (when-let [v (cell grid pos)]
                      [[pos dir 1] (+ cost v)]))
                  (let [dir (left dir)
                        pos (plus-dir pos dir)]
                    (when-let [v (cell grid pos)]
                      [[pos dir 1] (+ cost v)]))]))
       (remove nil?)
       (reduce (fn [m [k v]]
                 (update m k #(if % (min % v) v)))
               {})))

(let [grid (map-grid (fn [c _x _y] (parse-long (str c)))
                     (->2darray example))]
  (update-cost grid {[[0 0] [1 0] 0] 0}))

(defn part1
  "Solve part 1."
  [s]
  (let [grid (map-grid (fn [c _x _y] (parse-long (str c)))
                       (->2darray s))]
    (loop [cost {[[0 0] [1 0] 0] 0}]
      (let [nextcost (update-cost grid cost)]
        (if (= cost nextcost)
          (->> cost
               (filter (fn [[[pos _dir _num] _v]] (= pos [(dec (count (first grid))) (dec (count grid))])))
               (map (fn [[_k v]] v))
               (apply min))
          (recur nextcost))))))

(part1 example)

(part1 (slurp "input_17.txt"))

(defn update-cost2
  "Update the cost of each cell using the rules from part 2."
  [grid cost]
  (->> cost
       (mapcat (fn [[[pos dir num] cost]]
                 [[[pos dir num] cost]
                  (let [pos (plus-dir pos dir)]
                    (when-let [v (and (< num 10)
                                      (cell grid pos))]
                      [[pos dir (inc num)] (+ cost v)]))
                  (let [dir (right dir)
                        pos (plus-dir pos dir)]
                    (when-let [v (and (>= num 4)
                                      (cell grid pos))]
                      [[pos dir 1] (+ cost v)]))
                  (let [dir (left dir)
                        pos (plus-dir pos dir)]
                    (when-let [v (and (>= num 4)
                                      (cell grid pos))]
                      [[pos dir 1] (+ cost v)]))]))
       (remove nil?)
       (reduce (fn [m [k v]]
                 (update m k #(if % (min % v) v)))
               {})))

(defn part2
  "Solve part 2."
  [s]
  (let [grid (map-grid (fn [c _x _y] (parse-long (str c)))
                       (->2darray s))]
    (loop [cost {[[0 0] [1 0] 0] 0}]
      (let [nextcost (update-cost2 grid cost)]
        (if (= cost nextcost)
          (->> cost
               (filter (fn [[[pos _dir num] _v]]
                         (and (= pos [(dec (count (first grid))) (dec (count grid))])
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
