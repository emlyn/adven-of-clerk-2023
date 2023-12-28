;; # 🎄 Advent of Clerk: Day 23
(ns advent-of-clerk.day-23
  (:require [nextjournal.clerk]
            [emlyn.grid :as g]))

(def example "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

(defn neighbours
  "Neighbouring cells that can be reached from a given cell."
  [grid x y]
  (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [xx (+ x dx)
              yy (+ y dy)
              c (grid [x y])]
        :when (and (#{\. \< \> \^ \v} (grid [xx yy]))
                   (or (= \. c)
                       (#{[1 0 \>]
                          [-1 0 \<]
                          [0 1 \v]
                          [0 -1 \^]}
                        [dx dy c])))]
    [xx yy]))

(defn step-path
  "Advance a path one step in all possible directions
   (may result in multiple paths)."
  [graph [pos seen]]
  (for [next (remove seen (graph pos))]
    [next (conj seen next)]))

(defn part1
  "Solve part 1."
  [s]
  (let [grid (g/grid s)
        graph (into {}
                    (for [[x y] (keys grid)
                          :let [n (neighbours grid x y)]
                          :when (pos? (count n))]
                      [[x y] n]))
        start (->> (range (g/width grid))
                   (map (fn [x]
                          (when (= \. (grid [x 0]))
                            [x 0])))
                   (remove nil?))
        endy (dec (g/height grid))]
    (loop [paths (map (fn [p] [p #{p}]) start)
           best nil]
      (let [{next-paths false
             complete-paths true}
            (group-by (fn [[[_ y] _]] (= y endy))
                      (mapcat (partial step-path graph) paths))

            best (reduce (fn [best [p seen]]
                           (cond (nil? best) seen
                                 (> (count seen) (count best)) seen
                                 :else best))
                         best
                         complete-paths)]
        (if (seq next-paths)
          (recur next-paths best)
          (dec (count best)))))))

(part1 example)

#_(part1 (slurp "input_23.txt"))

(defn neighbours2
  "Neighbouring cells that can be reached from a given cell for part 2."
  [grid x y]
  (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [xx (+ x dx)
              yy (+ y dy)
              c (grid [xx yy])]
        :when (#{\. \< \> \^ \v} c)]
    [xx yy]))

(defn delete-node
  "Delete a node from the graph, merging its neighbours if possible."
  [graph pos]
  (let [nb (graph pos)
        graph (if (= 2 (count nb))
                (let [d (apply + (vals nb))
                      [n1 n2] (keys nb)]
                  (-> graph
                      (update n1 assoc n2 d)
                      (update n2 assoc n1 d)))
                graph)]
    (reduce (fn [gr [nb _]]
              (update gr nb dissoc pos))
            (dissoc graph pos)
            (graph pos))))

(delete-node
 {[0 0] {[0 1] 1}
  [0 1] {[0 0] 1
         [0 2] 1}
  [0 2] {[ 0 1] 1}}
 [0 1])

(defn prune-graph
  "Prune the graph by deleting nodes with only two neighbours."
  [graph ymax]
  (let [pruned (reduce (fn [gr pos]
                         (if (and (not (#{0 ymax} (second pos)))
                                  (= (count (gr pos)) 2))
                           (delete-node gr pos)
                           gr))
                       graph
                       (keys graph))]
    (if (= pruned graph)
      pruned
      (recur pruned ymax))))

(defn step-path2
  "Advance a path one step in all possible directions using part 2 rules
   (may result in multiple paths)."
  [graph [pos seen d]]
  (for [[next nd] (remove (comp seen first) (graph pos))]
    [next (conj seen next) (+ d nd)]))

(defn part2
  "Solve part 2."
  [s]
  (let [grid (g/grid s)
        graph  (into {}
                     (for [[x y] (keys grid)
                           :when (#{\. \< \> \^ \v} (grid [x y]))
                           :let [n (neighbours2 grid x y)]
                           :when (pos? (count n))]
                       [[x y] (into {} (map #(vector % 1) n))]))
        graph (prune-graph graph (dec (count grid)))
        start (->> (range (g/width grid))
                   (map (fn [x]
                          (when (= \. (grid [x 0]))
                            [x 0])))
                   (remove nil?))
        endy (dec (g/height grid))]
    (loop [paths (map (fn [p] [p #{p} 0]) start)
           best nil]
      (let [{next-paths false
             complete-paths true}
            (group-by (fn [[[_ y] _ _]] (= y endy))
                      (mapcat (partial step-path2 graph) paths))

            best (reduce (fn [best [_p _seen d]]
                           (cond (nil? best) (do (println "** best:" d) d)
                                 (> d best) (do (println "** best:" d) d)
                                 :else best))
                         best
                         complete-paths)]
        (if (seq next-paths)
          (recur next-paths best)
          best)))))

(part2 example)

#_(part2 (slurp "input_23.txt"))
