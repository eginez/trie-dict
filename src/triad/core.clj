(ns triad.core
  (:require [clojure.core.async :as as]
            [clojure.set :as st]
            [clojure.string :as sr])
  (:gen-class))

(def mat [[ "a" "b" ]["d" "e" ]])

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



(defn formWord [g [x y] word used]
  (let [newused (conj used [x y])
        n (st/difference (set (neighbours g [x y])) (set newused))] 
    (doseq  [l n]
      (let [newword  (str word (getat g l))]
        (println newword)
        (formWord g l newword newused)))))

(defn startWalk [mat]
  (doseq [x (range (count mat))
          y (range (count (nth mat x)))]
    (println (formWord mat [x y] (getat mat [x y]) []))))


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



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (neighbours mat 0 0)
  (neighbours mat [0 0])
  (startWalk mat)
  (println "starting to build dict triad")
  (build-triad-dict {} []))


