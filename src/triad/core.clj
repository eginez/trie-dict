(ns triad.core
  (:require [clojure.core.async :as as]
            [clojure.set :as st]
            [clojure.string :as sr])
  (:gen-class))

(def mat [[ "a" "b" "m" ]["d" "a" "p" ]["c" "a" "r"]])

(def english-words (sr/split-lines (slurp "/usr/share/dict/words")))

(defn in? [elem col]
  (some #(= elem %) col))

(defn getat[mat [x y]]
  (nth (nth mat y) x))

(defn valid-pos? [mat n]
  (and (< n (count mat)) (>= n 0)))

(defn neighbours [mat [a b]]
  (remove nil?
          (into []
                (for [x (range (dec a) (+ 2 a))
                      y (range (dec b) (+ 2 b))]
                  (if (and (not (and (= y b) (= x a))) (and (valid-pos? mat x) (valid-pos? mat y)))
                    [x y])))))

(defn all-words-from-position [g [x y] word used]
  (let [newused (conj used [x y])
        n (st/difference (set (neighbours g [x y])) (set newused))] 
    (flatten 
      (into []
            (for  [l n]
              (let [newword  (str word (getat g l))]
                ;(println newword)
                (conj (all-words-from-position g l newword newused) newword)))))))

(defn startWalk [mat]
  (doseq [x (range (count mat))
          y (range (count (nth mat x)))]
    (println (all-words-from-position mat [x y] (getat mat [x y]) []))))


(defn insert-word-in-triad [triad letters pos word]
  (if (< pos (count word))
    (let [letter (nth word pos)]
      (if (contains? (get-in triad letters) letter)
        (insert-word-in-triad triad (conj letters letter) (inc pos) word)
        (insert-word-in-triad (assoc-in triad (conj letters letter) {}) (conj letters letter) (inc pos) word)))
    triad))

(defn build-triad-dict [triad words]
  (if (empty? words)
    triad
    (recur (insert-word-in-triad triad [] 0 (first words)) (rest words))))

(defn find-words-trie [trie mat new-pos word used-positions]
  (cond 
    ;(nil? (get-in trie word)) (println (str "not a word: " word))
    (nil? (get-in trie word)) (identity word)
    (empty? (get-in trie word)) (println word)
    :else (let [all-neig-pos (st/difference (set (neighbours mat new-pos)) (set (conj used-positions new-pos)))]
            (doseq [pos all-neig-pos]
              (find-words-trie trie mat pos (str word (getat mat pos)) (conj used-positions new-pos))))))

(defn find-all-word-in-trie [trie mat]
  (doseq [x (range (count mat))
          y (range (count (nth mat x)))]
    (find-words-trie trie mat [x y] (getat mat [x y]) [])))

(def english-words-trie (build-triad-dict {} english-words))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (find-all-word-in-trie english-words-trie mat))
