(ns org.dipert.utils.math
  "These are some math-related functions I've found useful. 8-)"
  (:use (clojure.contrib math)))

(defn- powers-of [n]
  "For given base n, create lazy sequence of powers of n"
  (iterate #(* % n) 1))

(defn- char-range [& limits]
  "Given an even number of limit characters, creates an inclusive character sequence"
  (apply concat (map (fn [[start end]]
		       ;; Partition pairs, convert to ints, map back to chars
		       (map char (range (int start)
					(inc (int end))))) (partition 2 limits))))

(defn- syms-vals
  "Maps a base's representation characters to decimal values"
  [base]
  {:pre [(<= base 36) (> base 1)]}
  (let [syms (take base (char-range \0 \9 \A \Z))
	vals (range 0 (inc (count syms)))]
    (zipmap syms vals)))

(defn- prep-str [#^String s base]
  "Maps characters to values, ignoring unrecognized characters."
  (let [charmap (syms-vals base)]
    ;; Convert to upper case, filter out unknown chars, map to values
    (map charmap (filter #(contains? charmap %) (reverse (-> s .toUpperCase))))))

(defn strton [s base]
  "Given string s and int base, convert to integer"
  ;; Multiply pairs of powers and values, then sum them
  (reduce + (map (fn [[pow val]]
	  ;; Create pairs of powers and values
	  (* pow val)) (partition 2 (interleave (powers-of base) (prep-str s base))))))

(defmacro ccmp
  "Chained comparison, a la Python (and math). Requires an odd number of arguments, with interposed boolean binary operators."
  [& args]
  {:pre [(odd? (count args))]}
  `(and ~@(map (fn [[n1 f n2]] `(~f ~n1 ~n2)) (partition 3 2 args))))