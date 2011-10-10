(ns clojure99.list
  (:use [clojure.pprint])
  (:use [clojure.set])
  (:gen-class))

(defn my-last
  "Find the last box of a list."
  [coll]
  (loop [x nil [head & tail :as coll] coll]
    (cond
      (empty? coll) x
      :else (recur head tail))))

(defn my-but-last
  "Find the last but one box of a list."
  [coll]
  (loop [x nil y nil [head & tail :as coll] coll]
    (cond
      (empty? coll) x
      :else (recur y head tail))))

(defn element-at
  "Find the K'th element of a list."
  [coll k]
  (loop [k k [head & tail :as coll] coll]
    (cond
      (= 1 k) head
      :else (recur (dec k) tail))))

(defn length
  "Find the number of a list."
  [coll]
  (loop [n 0 coll coll]
    (cond
      (empty? coll) n
      :else (recur (inc n) (rest coll)))))

(defmulti my-reverse
  "Reverse a list."
  class)

(defmethod my-reverse String
  [string]
  (reduce str (my-reverse (seq string))))

(defmethod my-reverse :default
  [coll]
  (loop [acc '() [head & tail :as coll] coll]
    (cond
      (empty? coll) acc
      :else (recur (cons head acc) tail))))

(defn parindrome?
  "Find out whether a list is a palindrome."
  [coll]
  (= coll (my-reverse coll)))

(defn my-flatten
  "Flatten a nested list structure"
  [coll]
  (loop [acc [] [head & tail :as coll] coll]
    (cond
      (empty? coll) acc
      (coll? head) (recur (into acc (my-flatten head)) tail)
      :else (recur (conj acc head) tail))))

(defn compress
  "Eliminate consective duplicate of list elements."
  [coll]
  (loop [acc [] [head & tail :as coll] coll]
    (cond
      (empty? coll) acc
      :else (recur (conj acc head) (drop-while #(= head %) tail)))))

(defn pack
  "Pack consective duplicate of list elements into sublists."
  [coll]
  (loop [acc [] [head & tail :as coll] coll]
    (cond
      (empty? coll) acc
      :else (recur (conj acc (take-while #(= head %) coll)) (drop-while #(= head %) coll)))))

(defn- comprss [coll]
  [(count coll) (first (coll))])

(defn encode
  "Run-length encoding of a list."
  [coll]
  (map #(list (count %) (first %)) (pack coll)))

(defn encode-modified
  "Modified run-length encoding."
  [coll]
  (map #(if (= 1 (first %)) (second %) %) (encode coll)))

(defn decode
  "Decode a run-length encoded list."
  [coll]
  (flatten
    (loop [acc [] [head & tail :as coll] coll]
      (cond
        (empty? coll) acc
        :else (recur (conj acc (if (seq? head) (repeat (first head) (second head)) head)) tail)))))

(defn encode-direct
  "Run-length encoding of a list (direct solution)."
  [coll]
  (loop [acc [] [head & tail :as coll] coll]
    (cond
      (empty? coll) acc
      :else (recur
              (conj acc (if (= 1 (count (take-while #(= head %) coll)))
                          head
                          (list (count (take-while #(= head %) coll)) (first (take-while #(= head %) coll)))))
              (drop-while #(= head %) coll)))))

(defn dupli
  "Duplicate the elements of a list."
  [coll]
  (mapcat #(repeat 2 %) coll))

(defn repli
  "Replicate the elements of a list a given number of times."
  [coll n]
  (mapcat #(repeat n %) coll))

(defn drop-nth
  "Drop every N'th element from a list."
  [coll n]
  (loop [acc [] [head & tail :as coll] coll i n]
    (cond
      (empty? coll) (sequence acc)
      (= i 1) (recur acc tail n)
      :else (recur (conj acc head) tail (dec i)))))

(defn split
  "Split a list into two parts; the length of the first part is given."
  [coll n]
  (list (take n coll) (drop n coll)))

(defn slice
  "Extract a slice from a list."
  [coll n m]
  (drop (dec n) (take m coll)))

(defn rotate
  "Rotate a list N places to the left."
  [coll n]
  (cond
    (< n 0) (rotate coll (+ n (count coll)))
    :else (let [[fst scd] (split coll n)]
            (concat scd fst))))

(defn remove-at
  "Remove the K'th element from a list."
  [coll n]
  (loop [acc [] [head & tail :as coll] coll n n]
    (cond
      (empty? coll) acc
      (= n 1) (recur acc tail (dec n))
      :else (recur (conj acc head) tail (dec n)))))

(defn insert-at
  "Insert an element at a given position into a list."
  [e coll n]
  (loop [acc [] [head & tail :as coll] coll i n]
    (cond
      (empty? coll) (concat acc (list e))
      (= 1 i) (concat acc (list e) coll)
      :else (recur (conj acc head) tail (dec i)))))

(defn my-range
  "Create a list containing all integers within a given range."
  [n m]
  (loop [acc [] i n]
    (cond
      (< m i) acc
      :else (recur (conj acc i) (inc i)))))

(defn rnd-select
  "Extract a given number of randomly selected elements from a list."
  [coll n]
  (loop [acc [] coll coll n n]
    (cond
      (= n 0) acc
      :else (let [i (rand-int (count coll))]
              (recur (conj acc (nth coll i)) (remove-at coll (inc i)) (dec n))))))

(defn lotto-select
  "Lotto: Draw N different random numbers from the set 1..M."
  [n m]
  (rnd-select (my-range 1 m) n))

(defn rnd-permu
  "Generate a random permutation of the elements of a list."
  [coll]
  (rnd-select coll (count coll)))

(defn combination
  "Generate the combinations of K distinct objects chosen from the N elements of a list."
  [n coll]
  (let [[head & tail] coll]
    (cond 
      (zero? n) [[]]
      (empty? coll) []
      :else (concat
              (map #(cons head %) (combination (dec n) tail))
              (combination n tail)))))

(defn- diff-list
  [ls lg]
  (filter (fn [x] (not-any? #(= x %) ls)) lg))

; (defn group
;   "Group the elements of a set into disjoint subsets."
;   nil)
; 
; (defn group3 [coll]
;   (group '(2 3 4) coll))

(defn lsort
  "Sorting a list of lists according to length of sublists.
  a) We suppose that a list contains elements that are lists themselves.
  The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa."
  [coll]
  (sort-by count < coll))

(defn lfsort
  "Sorting a list of lists according to length of sublists.
  b) Again, we suppose that a list contains elements that are lists themselves.
  But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later."
  [coll]
  (defn count-list [coll]
    (loop [acc [] [head & tail :as coll] coll]
      (cond
        (empty? coll) acc
        :else (recur (conj acc (list (count head) head)) tail))))
  (defn count-map [coll]
    (loop [acc {} [head & tail :as coll] coll]
      (cond
        (empty? coll) acc
        :else (recur (assoc acc (first head) (conj (get acc (first head) []) (second head))) tail))))
  (mapcat second (sort-by #(count (second %)) < (count-map (count-list coll)))))
