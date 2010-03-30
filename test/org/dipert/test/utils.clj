(ns org.dipert.test.utils
  (:use clojure.test org.dipert.utils))

(with-private-vars [org.dipert.utils [syms-vals prep-str]]

  (deftest test-syms-vals
    (let [base-10-syms (syms-vals 10)]
      (are [x y] (= x y)
	   10 (count base-10-syms)
	   2  (base-10-syms \2)
	   45 (reduce + (vals base-10-syms)) "Powers of 10 should have these properties")
      (are [x] (thrown? AssertionError (syms-vals x))
	   123
	   0
	   -34) "Should throw AssertionErrors"))
  
  (deftest test-prep-str
    (are [x y] (= x y)
	 3  (count (prep-str "abc" 16))
	 12 (first (prep-str "10c" 16))
	 1  (last  (prep-str "10c" 16))
	 3  (count (prep-str "azbxc" 16))) "Should match the correct representation strings"))

(deftest test-powers-of
  (let [powers-2 (powers-of 2)
	powers-8 (powers-of 8)
	powers-10 (powers-of 10)]
    (are [x y] (= x y)
	 1024   (nth powers-2 10)
	 73     (reduce + (take 3 powers-8))
	 100000 (nth powers-10 5)) "Powers of 2,8,10 should have these properties"))

(deftest test-strton
  (are [n base] (= 3764 (strton n base))
       "0111010110100" 2
       "7264"          8
       "EB4"           16
       "eb4"           16
       "zeb4q"         16
       "0xEB4"         16
       "2912"          11 "Should all be equal 3764"))

(deftest test-ccmp
  (are [x y] (= x y)
       true  (ccmp 1 < 2 < 3 < 100 <= 433)
       false (ccmp 1 < 0)
       false (ccmp 1 >= 0 < 10 > 200)
       true  (ccmp 5 < 10 = 10) "Should result in all of these booleans"))

(deftest test-char-range
  (is (= [\A \B \C \0 \1 \2] (char-range \A \C \0 \2)))
  (is (= (map #(char (+ 97 %)) (range 0 26)) (char-range \a \z))))

(deftest test-rands-from
  (let [from-set #{:A :B :C}]
    (are [x y] (= x y)
	 100 (count (take 100 (rands-from from-set)))
	 nil ((set (take 10 (rands-from from-set))) :D)) "Random members of the set should satisfy these properties"))

(deftest test-rand-str
  (let [randstr (rand-str 10 (char-range \a \z \A \Z))]
    (is (= 10 (count randstr)) "Should be 10 characters long")
    (is (every? char? randstr) "Every member should be a char")
    (is (not-any? #(Character/isWhitespace %)
		  randstr) "There should be no whitespace")
    (is (every? #(and (>= (int %) (int \A))
		      (<= (int %) (int \z)))
		randstr)) "Every member should be greater than A and less than z"))

(deftest test-using-iota
  (is (= 3 (using-iota
	(+ iota iota iota)))))