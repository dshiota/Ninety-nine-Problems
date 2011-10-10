(in-ns 'clojure99.test.core)

(testing
  "P01"
  (is (= :d (my-last '(:a :b :c :d))))
  (is (= 4 (my-last [1 2 3 4])))
  (is (= \g (my-last "test string"))))

(testing
  "P02"
  (is (nil? (my-but-last '())))
  (is (nil? (my-but-last '(:a))))
  (is (= :c (my-but-last '(:a :b :c :d))))
  (is (= 3 (my-but-last [1 2 3 4])))
  (is (= \n (my-but-last "test string"))))

(testing
  "P03"
  (is (nil? (element-at '() 2)))
  (is (nil? (element-at '(:a :b :c :d) 5)))
  (is (= :c (element-at '(:a :b :c :d :e) 3)))
  (is (= \s (element-at "test string" 3))))

(testing
  "P04"
  (is (= 5 (length '(:a :b :c :d :e))))
  (is (= 11 (length "test string"))))

(testing
  "P05"
  (is (= [5 4 3 2 1] (my-reverse [1 2 3 4 5]))))

(testing "P06"
  (is (= false (parindrome? "string")))
  (is (= true (parindrome? "abcba")))
  (is (= true (parindrome? "xamax")))
  (is (= false (parindrome? "test"))))

(testing
  "P07"
  (is (= '(:a :b :c :d :e) (my-flatten '(:a (:b (:c :d) :e))))))

(testing
  "P08"
  (is (= '(:a :b :c :a :d :e) (compress '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)))))

(testing
  "P09"
  (is (= '((:a :a :a :a) (:b) (:c :c) (:a :a) (:d) (:e :e :e :e)) 
         (pack '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)))))

(testing
  "P10"
  (is (= '((4 :a) (1 :b) (2 :c) (2 :a) (1 :d) (4 :e))
         (encode '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)))))

(testing
  "P11"
  (is (= '((4 :a) :b (2 :c) (2 :a) :d (4 :e))
         (encode-modified '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)))))

(testing
  "P12"
  (is (= '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)
         (decode '((4 :a) :b (2 :c) (2 :a) :d (4 :e))))))

(testing
  "P13"
  (is (= '((4 :a) :b (2 :c) (2 :a) :d (4 :e))
         (encode-direct '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)))))

(testing
  "P14"
  (is (= '(:a :a :b :b :c :c :d :d :e :e) (dupli '(:a :b :c :d :e)))))

(testing
  "P15"
  (is (= '(:a :a :a :b :b :b :c :c :c) (repli '(:a :b :c) 3))))

(testing
  "P16"
  (is (= '(:a :b :d :e :g :h :k)
         (drop-nth '(:a :b :c :d :e :f :g :h :i :k) 3))))

(testing
  "P17"
  (is (= '((:a :b :c) (:d :e :f :g :h :i :k))
         (split '(:a :b :c :d :e :f :g :h :i :k) 3))))

(testing
  "P18"
  (is (= '(:c :d :e :f :g)
         (slice '(:a :b :c :d :e :f :g :h :i :k) 3 7))))

(testing
  "P19"
  (is (= '(:d :e :f :g :h :a :b :c)
         (rotate '(:a :b :c :d :e :f :g :h) 3)))
  (is (= '(:g :h :a :b :c :d :e :f)
         (rotate '(:a :b :c :d :e :f :g :h) -2))))

(testing
  "P20"
  (is (= '(:a :c :d)
         (remove-at '(:a :b :c :d) 2))))

(testing
  "P21"
  (is (= '(:a :alpha :b :c :d)
         (insert-at :alpha '(:a :b :c :d) 2)))
  (is (= '(:a :b :c :d :alpha)
         (insert-at :alpha '(:a :b :c :d) 6))))

(testing
  "P22"
  (is (= '(4 5 6 7 8 9)
         (my-range 4 9))))

(testing
  "P23"
  (is (= 3 (count (rnd-select '(:a :b :c :d :e :f :g :h) 3)))))

(testing
  "P24"
  (is (= 6 (count (lotto-select 6 49)))))

(testing
  "P25"
  (is (== 6 (count (rnd-permu '(:a :b :c :d :e :f))))))

(testing
  "P26"
  (is (= 20 (count (combination 3 '(:a :b :c :d :e :f))))))
  
; (testing
;   "P27"
;   (let [memberList #{:aldo :beat :carla :david :evi :flip :gary :hugo :ida}]
;     (pprint (group3 memberList))))
    ;(is (= (group3 memberList) (group '(2 3 4) memberList)))

(testing
  "P28"
  (let [testedList '((:a :b :c) (:d :e) (:f :g :h) (:d :e) (:i :j :k :l) (:m :n) (:o))]
    (is (= '((:o) (:d :e) (:d :e) (:m :n) (:a :b :c) (:f :g :h) (:i :j :k :l))
           (lsort testedList)))
    ; (is (= '((:i :j :k :l) (:o) (:a :b :c) (:f :g :h) (:d :e) (:d :e) (:m :n))
    (is (= '((:o) (:i :j :k :l) (:a :b :c) (:f :g :h) (:d :e) (:d :e) (:m :n))
           (lfsort testedList)))))
