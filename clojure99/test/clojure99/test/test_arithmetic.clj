(in-ns 'clojure99.test.core)

(testing
  "P31"
  (is-prime? 7))

(testing
  "P32"
  (is (= 43 (my-gcd 1333 5461)))
  (is (= 9 (my-gcd 36 63))))

(testing
  "P33"
  (is (= false (coprime? 5461 1333)))
  (is (= true (coprime? 35 64))))

(testing
  "P34"
  (is (= 4 (totient-phi 10))))

(testing
  "P35"
  (is (= '(3 3 5 7) (prime-factors 315))))

(testing
  "P36"
  (is (= '((3 2) (5 1) (7 1)) (prime-factors-multi 315))))

(testing
  "P37"
  (is (= 96 (totient 97)))
  (is (= 60 (totient 99)))
  (is (= 4 (totient 10))))

(testing
  "P38"
  (dotimes [n 10]
    (time (totient-phi 10090)))
  (dotimes [n 10]
    (time (totient 10090))))

(testing
  "P39"
  (is (= '(5 7 11 13 17 19)
         (primes-in-range 4 20))))

(testing
  "P40"
  (is (= '(5 23)
         (goldbach 28))))

(testing
  "P41"
  (goldbach-list 9 20)
  (goldbach-list 1 2000 50))
