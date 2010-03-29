(ns org.dipert.test.utils
  (:use (clojure.test)
	(org.dipert.utils)))

(with-private-vars [org.dipert.utils [syms-vals char-range prep-str]]

  (deftest test-syms-vals
    (let [base-10-syms (syms-vals 10)]
      (are [x y] (= x y)
	   10 (count base-10-syms)
	   2 (base-10-syms \2)
	   45 (reduce + (vals base-10-syms)))
      (are [x] (thrown? AssertionError x)
	   (syms-vals 123)
	   (syms-vals 0)
	   (syms-vals -34))))
  
  (deftest test-prep-str
    (are [x y] (= x y)
	 3 (count (prep-str "abc" 16))
	 12 (first (prep-str "10c" 16))
	 1 (last (prep-str "10c" 16))
	 3 (count (prep-str "azbxc" 16)))))

(deftest test-powers-of
  (let [powers-2 (powers-of 2)
	powers-8 (powers-of 8)
	powers-10 (powers-of 10)]
    (are [x y] (= x y)
	 1024 (nth powers-2 10)
	 73 (reduce + (take 3 powers-8))
	 100000 (nth powers-10 5))))

(deftest test-strton
  (are [x y] (= x y)
       3764 (strton "0111010110100" 2)
       3764 (strton "7264" 8)
       3764 (strton "EB4" 16)
       3764 (strton "eb4" 16)
       3764 (strton "zeb4q" 16)
       3764 (strton "0xEB4" 16)
       717 (strton "5a2" 11)))

(deftest test-ccmp
  (are [x y] (= x y)
       true (ccmp 1 < 2 < 3 < 100 <= 433)
       false (ccmp 1 < 0)
       false (ccmp 1 >= 0 < 10 > 200)
       true (ccmp 5 < 10 = 10)))

(deftest test-char-range
  (is (= [\A \B \C \0 \1 \2] (char-range \A \C \0 \2)))
  (is (= (map #(char (+ 97 %)) (range 0 26)) (char-range \a \z))))

(deftest test-rands-from
  (let [from-set #{:A :B :C}]
    (are [x y] (= x y)
	 100 (count (take 100 (rands-from from-set)))
	 nil ((set (take 10 (rands-from from-set))) :D))))

(deftest test-rand-str
  (let [randstr (rand-str 10 (char-range \a \z \A \Z))]
    (is (= 10 (count randstr)))
    (is (every? char? randstr))
    (is (not-any? #(Character/isWhitespace %)
		  randstr))
    (is (every? #(and (>= (int %) (int \A))
		      (<= (int %) (int \z)))
		randstr))))