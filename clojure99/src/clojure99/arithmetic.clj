(ns clojure99.arithmetic
  (:use [clojure.pprint])
  (:use [clojure.contrib.lazy-seqs])
  (:use [clojure.contrib.math])
  (:gen-class))

(defn is-prime?
  "Determine whether a given integer number is prime."
  [n]
  (not-any? #(= 0 (mod n %)) (take-while #(<= % (sqrt  n)) primes)))

(defn my-gcd
  "Determine the greatest common divisor of two positive integer numbers."
  [n m]
  (cond
    (< n m) (gcd m n)
    (= m 0) m
    :else (gcd m (- n m))))

(defn coprime?
  "Determine whether two positive integer numbers are coprime."
  [n m]
  (= 1 (my-gcd n m)))

(defn totient-phi
  "Calculate Euler's totient function phi(m)."
  [n]
  (count (filter #(coprime? % n) (range 1 n))))

(defn prime-factors
  "Determine the prime factors of a given positive integer."
  [n]
  (loop [acc [] n n]
    (cond
      (= 1 n) acc
      :else (let [f (first (filter #(= 0 (mod n %)) primes))]
              (recur (conj acc f) (/ n f))))))

(defn prime-factors-multi
  "Determine the prime factors of a given positive integer (2)."
  [n]
  (loop [acc [] [head & tail :as coll] (prime-factors n)]
    (cond
      (empty? coll) acc
      :else (recur 
              (conj acc (list head (count (take-while #(= head %) coll))))
              (drop-while #(= head %) coll)))))

(defn totient
  "Calculate Euler's totient function phi(m) (improved)."
  [n]
  (int (reduce *
               1 
               (map (fn [[p m]] (* (dec p) (Math/pow p (dec m))))
                    (prime-factors-multi n)))))

(defn primes-in-range
  "A list of prime numbers."
  [m n]
  (drop-while #(<= % m) (take-while #(<= % n) primes)))

(defn goldbach
  "Goldbach's conjecture."
  [n]
  (loop [[head & tail :as coll] (primes-in-range 2 n)]
    (cond
      (empty? coll) nil
      (is-prime? (- n head)) (list head (- n head))
      :else (recur tail))))

(defn goldbach-list
  "A list of Goldbach compositions."
  ([start end] (goldbach-list start end 1))
  ([start end minimum]
   (let [evens (filter even? (range start (inc end)))]
     (dorun (for [l (filter #(< minimum (first %)) (remove #(nil? %)
                                                           (map goldbach (filter even? (range start (inc end))))))]
              (do (println (+ (first l) (second l)) "=" (first l) "+" (second l))))))))
