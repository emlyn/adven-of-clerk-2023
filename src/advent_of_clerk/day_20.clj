;; # 🎄 Advent of Clerk: Day 20
(ns advent-of-clerk.day-20
  (:require [nextjournal.clerk]
            [clojure.string :as str]))

(def example "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(def example2 "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")

(defn parse-input
  [s]
  (let [tmp
        (->> s
             str/split-lines
             (map (partial re-find #"^([^a-z]?)([a-z]+) *-> *([a-z, ]*)"))
             (map (fn [[_ type name dests]]
                    [name {:type type
                           :out (str/split dests #", *")}]))
             (into {}))]
    (reduce-kv (fn [m name {:keys [out]}]
                 (reduce (fn [m dest]
                           (let [type (get-in m [dest :type])]
                             (case type
                               "&" (update-in m [dest :state] assoc name 0)
                               "%" (assoc-in m [dest :state] 0)
                               m)))
                         m
                         out))
               tmp
               tmp)))

(parse-input example)

(defmulti process-module (fn pm-multi [m _] (:type m)))

(def output (atom 0))

(defmethod process-module nil ;; missing module (e.g. output in example2)
  [_ [sign]]
  (when (zero? sign)
    (swap! output inc))
  [nil []])

(defmethod process-module "%" ;; flip-flop
  [{:keys [out] :as module} [sign _ self]]
  (if (zero? sign)
    (let [module (update module :state #(- 1 %))]
      [module (map #(vector (:state module) self %) out)])
    [module []]))

(defmethod process-module "&" ;; conjunction
  [{:keys [out] :as module} [sign src self]]
  (let [module (assoc-in module [:state src] sign)
        signal (if (every? #(= 1 %) (vals (:state module)))
                 0
                 1)]
    [module (map #(vector signal self %) out)]))

(defmethod process-module "" ;; broadcaster
  [{:keys [out] :as module} [sign _ self]]
  [module (map (fn pmb [o] (vector sign self o)) out)])

(defmethod process-module :default
  [c]
  (throw (Exception. (str "Unknown module type: " c))))

(def press (atom 0)) ;; counts number of presses of button

(defn process-button
  [network]
  (swap! press inc)
  (loop [network network
         queue (conj clojure.lang.PersistentQueue/EMPTY [0 "button" "broadcaster"])
         num {}]
    (if-let [[sign _ dest :as pulse] (peek queue)]
      (let [[module pulses] (process-module (network dest) pulse)]
        ;; For part 2, find period of each module feeding the output module:
        #_(when (and (zero? sign) (#{"kv" "jg" "rz" "mr"} dest))
          (println ">pulse" dest @press))
        (recur (assoc network dest module)
               (reduce conj (pop queue) pulses)
               (update num sign (fnil inc 0))))
      [network num])))

(process-button (parse-input example))

(defn part1
  [s]
  (loop [network (parse-input s)
         remaining 1000
         num {}]
    (if (zero? remaining)
      (apply * (vals num))
      (let [[network newnum] (process-button network)]
        (recur network
               (dec remaining)
               (merge-with + num newnum))))))

(part1 example)

(part1 example2)

#_(part1 (slurp "input_20.txt"))

(defn part2
  [s]
  (reset! output 0)
  (reset! press 0)
  (loop [network (parse-input s)
         num 1]
    (if (zero? @output)
      (let [[network _] (process-button network)]
        (recur network (inc num)))
      num)))

#_(part2 (slurp "input_20.txt"))

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

;; Find lowest common multiple of periods of modules feeding output:
(reduce lcm [4003 4073 3911 3739])
