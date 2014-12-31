(ns triad.core
  (:require [clojure.core.async :as as]
            [clojure.set :as st]
            [clojure.string :as sr])
  (:gen-class))

(def mat5 [[ "a" "c" "m" "b" "d"]["r" "a" "p" "n" "x" ] ["f" "e" "d" "f" "e"] ["r" "a" "z" "p" "n"]["c" "t" "o" "l" "f"]])
(def mat4 [[ "a" "c" "m" "b"  ]["r" "a" "p" "n" ] ["f" "e" "d" "f"] ["r" "a" "z" "p"]])
(def mat3 [[ "a" "c" "m" ]["r" "a" "p"] ["f" "e" "d"]])

(def english-words (sr/split-lines (slurp "/usr/share/dict/words")))


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
    (assoc-in triad word (merge (get-in triad word) {:isWord true}))))

(defn build-triad-dict [triad words]
  (if (empty? words)
    triad
    (recur (insert-word-in-triad triad [] 0 (first words)) (rest words))))

(defn find-words-trie [trie mat new-pos word used-positions]
  (cond 
    ;(nil? (get-in trie word)) (println (str "not a word: " word))
    (nil? (get-in trie word)) (identity word)
    (empty? (get-in trie word)) (println word)
    :else (do 
            (if (get-in trie (conj (into [] (seq word)) :isWord)) (println word))
            (let [all-neig-pos (st/difference (set (neighbours mat new-pos)) (set (conj used-positions new-pos)))]
              (doseq [pos all-neig-pos]
                (find-words-trie trie mat pos (str word (getat mat pos)) (conj used-positions new-pos)))))))

(defn find-all-words-in-dic [dic mat]
  (doseq [x (range (count mat))
          y (range (count (nth mat x)))]
    (println (filter (complement nil?) (map #(get dic %) (all-words-from-position mat [x y] (getat mat [x y]) []))))))

  

(defn find-all-word-in-trie [trie mat]
  (doseq [x (range (count mat))
          y (range (count (nth mat x)))]
    (find-words-trie trie mat [x y] (getat mat [x y]) [])))

(defn build-dict-english-words [word-list]
  (into {}
        (for [word word-list]
          (hash-map word word))))

(defn build-trie-words [word-list]
  (build-triad-dict {} word-list))

(def english-dic (build-dict-english-words english-words))
(def english-trie (build-trie-words english-words))

(defn benchmark-finds [mat]
  (time (find-all-word-in-trie english-trie mat))
  (time (find-all-words-in-dic english-dic mat)))

  
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (time (find-all-word-in-trie english-trie mat3))
  (time (find-all-words-in-dic english-dic mat3)))
