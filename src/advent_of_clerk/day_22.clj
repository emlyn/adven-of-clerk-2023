;; # 🎄 Advent of Clerk: Day 22
(ns advent-of-clerk.day-22
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn parse-input
  "Parse input string to seq of [[x1 y1 z1] [x2 y2 z2]] bricks."
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #"~"))
       (map #(mapv (fn [s] (mapv parse-long (str/split s #","))) %))))

(parse-input example)

(defn place-brick
  "Place brick down at its current x-y position until it is resting on another brick or the ground."
  [tops [[x1 y1 z1] [x2 y2 z2]]]
  (let [height ;; highest point under this brick
        (->> (for [x (range x1 (inc x2))
                   y (range y1 (inc y2))]
               (last (last (tops [x y]))))
             (remove nil?)
             (apply max 0))
        thickness (- z2 z1)
        dropped [[x1 y1 (inc height)] [x2 y2 (+ 1 height thickness)]]]
    [(into tops (for [x (range x1 (inc x2))
                      y (range y1 (inc y2))]
                  [[x y] dropped]))
     [dropped (set (for [x (range x1 (inc x2))
                         y (range y1 (inc y2))
                         :let [[[_ _ _] [_ _ z2] :as support] (tops [x y])]
                         :when (= height z2)]
                     support))]]))

(place-brick {} [[1 0 1] [1 2 1]])
(place-brick {[1 1] [[1 1 1] [1 1 1]]} [[1 0 1] [1 2 1]])

(defn place-bricks
  "Place bricks on top of each other, returning the final state.
   `tops` is a map of x-y coordinate to topmost brick at that coordinate
   the height at that coordinate is equal to the z2-coordinate of the brick.
   `bricks` is a seq of tuples of brick and a set of all bricks it is resting on."
  [tops bricks]
  (reduce (fn [[tops bricks] brick]
            (let [[t b] (place-brick tops brick)]
              [t (conj bricks b)]))
          [tops []]
          bricks))

(place-bricks {} [[[1 0 1] [1 2 1]]
                  [[0 0 2] [2 0 2]]])

(place-bricks {} [[[1 0 1] [1 2 1]]
                  [[0 0 2] [2 0 2]]
                  [[0 2 3] [2 2 3]]])

(defn brick-info
  [s]
  (let [[_tops bricks] (->> s
                            parse-input
                            (sort-by (fn [[[_ _ z1] []]] z1))
                            (place-bricks {}))

        bricks ;; re-shape into map
        (into {}
              (map (fn [[b s]] [b {:supported-by s :supports #{}}])
                   bricks))]

    ;; fill in :supports info
    (reduce-kv (fn [bricks brick {:keys [supported-by]}]
                 (reduce (fn [bricks supported-by]
                           (update-in bricks [supported-by :supports] conj brick))
                         bricks
                         supported-by))
               bricks
               bricks)))

(defn part1
  "Solve part 1."
  [s]
  (let [bricks (brick-info s)]
    (->> bricks
         ;; remove any bricks that support a brick that is only supported by one brick
         (remove (fn [[_brick {:keys [supports]}]]
                   (some (fn [b] (->> b bricks :supported-by count (= 1)))
                         supports)))
         count)))

(part1 example)

#_(part1 (slurp "input_22.txt"))

(defn chain-size
  "How many other bricks would fall of we disintegrate `brick`?"
  [bricks [brick _]]
  (loop [gone #{brick}]
    (if-let [more-gone (->> bricks
                            (filter (fn [[_b {:keys [supported-by]}]]
                                      (and (seq supported-by)
                                           (every? gone supported-by))))
                            (map first)
                            (remove gone)
                            seq)]
      (recur (into gone more-gone))
      (dec (count gone)))))

(defn part2
  "Solve part 2."
  [s]
  (let [bricks (brick-info s)]
    (->> bricks
         (map (partial chain-size bricks))
         (apply +))))

(part2 example)

#_(part2 (slurp "input_22.txt"))
