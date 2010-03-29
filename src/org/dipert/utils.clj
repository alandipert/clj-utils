(ns org.dipert.utils
  "These are some functions I've found useful. 8-)"
  (:use clojure.contrib.math))

(defn powers-of
  "For given base n, create lazy sequence of powers of n"
  [n]
  (iterate #(* % n) 1))

(defn char-range
  "Given an even number of limit characters,
   creates an inclusive character sequence"
  [& limits]
  {:pre [(even? (count limits))]}
  (map char (mapcat (fn [[start end]]
		      (range (int start) (inc (int end)))) (partition 2 limits))))

(defn rands-from
  "Lazy sequence of random members of from-set"  
  [from-set]
  (let [from (vec from-set)]
    (lazy-seq (cons (from (rand-int (count from))) (rands-from from)))))

(defn rand-str
  "Random string of length len composed of items
   from from-chars"
  [len from-chars]
  (apply str (take len (rands-from from-chars))))

(defn- syms-vals
  "Maps a base's representation characters to 
   decimal values"
  [base]
  {:pre [(<= base 36) (> base 1)]}
  (zipmap (take base (char-range \0 \9 \A \Z))
	  (iterate inc 0)))

(defn- prep-str
  "Maps characters to values, ignoring unrecognized 
   characters."
  [#^String s base]
  (let [charmap (syms-vals base)]
    (map charmap (filter #(contains? charmap %) (reverse (-> s .toUpperCase))))))

(defn strton
  "Given string s and int base, convert to integer"  
  [s base]
  (reduce + (map (fn [[pow val]]
	  (* pow val)) (partition 2 (interleave (powers-of base) (prep-str s base))))))

(defmacro ccmp
  "Chained comparison, a la Python (and math). 
   Requires an odd number of arguments, with 
   interposed boolean binary operators."
  [& args]
  {:pre [(odd? (count args))]}
  `(and ~@(map (fn [[n1 f n2]] `(~f ~n1 ~n2)) (partition 3 2 args))))

(defmacro with-private-vars [[ns vars] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `@(ns-resolve '~ns '~%2)) [] vars)
     ~@tests))