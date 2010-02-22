(ns org.dipert.utils.test.math
  (:use [org.dipert.utils.math])
  (:use [clojure.test]))

(defmacro with-private-vars [[ns vars] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `@(ns-resolve '~ns '~%2)) [] vars)
     ~@tests))

(with-private-vars [org.dipert.utils.math [powers-of syms-vals char-range prep-str]]

  (deftest test-powers-of
    (let [powers-2 (powers-of 2)
	  powers-8 (powers-of 8)
	  powers-10 (powers-of 10)]
      (is (= 1024 (nth powers-2 10)) "The 10th power of 2 should be 1024")
      (is (= 73 (reduce + (take 3 powers-8))) "The sum of the first 3 powers of 8 should be 73")
      (is (= 100000 (nth powers-10 5)))) "The sum of the first 5 powers of 10 should be 100000")

  (deftest test-syms-vals
    (let [base-10-syms (syms-vals 10)]
      (is (= 10 (count base-10-syms)) "The number of keys should be 10")
      (is (= 2 (base-10-syms \2)) "The key \\2 should map to the number 2")
      (is (= 45 (reduce + (vals base-10-syms))) "The sum of the decimal values should be 45")
      (is (thrown? AssertionError (syms-vals 123)) "A base larger than 36 should throw AssertionError")
      (is (thrown? AssertionError (syms-vals 0)) "A zero base should throw AssertionError")
      (is (thrown? AssertionError (syms-vals -34)) "A negative base should throw AssertionError")))
  
  (deftest test-prep-str
    (is (= 3 (count (prep-str "abc" 16))) "The number of values in the hex string 'abc' should be 3")
    (is (= 12 (first (prep-str "10c" 16))) "The first value of the hex string '10c' should be 12 (c=12)")
    (is (= 1 (last (prep-str "10c" 16))) "The last value of the hex string '10c' should be 1 (1=1)")
    (is (= 3 (count (prep-str "azbxc" 16))) "Non-hex chars in input should be ignored")))

(deftest test-strton
  (is (= 3764 (strton "0111010110100" 2)) "The binary string should equal 3764 in decimal")
  (is (= 3764 (strton "7264" 8)) "The octal string should equal 3764 in decimal")
  (is (= 3764 (strton "EB4" 16)) "The hex string should equal 3764 in decimal")
  (is (= 3764 (strton "eb4" 16)) "The hex string should equal 3764 in decimal")
  (is (= 3764 (strton "zeb4q" 16)) "The corrupt hex string should equal 3764 in decimal")
  (is (= 3764 (strton "0xEB4" 16)) "The hex string with a preceding 0x should equal 3764 in decimal")
  (is (= 717 (strton "5a2" 11))) "The undecimal string should equal 717 in decimal")

(deftest test-ccmp
  (is (= true (ccmp 1 < 2 < 3 < 100 <= 433)) "One is less than two is less than three is less than 100 is lte 433")
  (is (= false (ccmp 1 < 0)) "One should not be less than zero")
  (is (= false (ccmp 1 >= 0 < 10 > 200)) "Bonkers to expect this to work")
  (is (= true (ccmp 5 < 10 = 10)) "The equality operator is in the mix"))
