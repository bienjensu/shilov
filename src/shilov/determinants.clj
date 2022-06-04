(ns shilov.determinants)

(defn zero-squarevec [n]
  "Returns a square vector of nxn size filled with zeroes."
  (vec (repeat n
               (vec (repeat n 0)))))

(defn rand-linevec [n m]
  "Returns a line vector of n size filled with random ints between 0 and m."
  (vec (repeatedly n #(rand-int m))))

(defn rand-squarevec [n m]
  "Returns a square vector of nxn size filled with random ints between 0 and m."
  (vec (repeatedly n #(rand-linevec n m))))

(defn Nperms [c]
  "Returns the number of permutations of the line vector of ints c."
  (letfn [(rperms [x] (count (remove false?
                               (map (fn [y] (< y (last x))) (butlast x)))))]
    (let [subvecs
          (reductions conj [] (reverse c))]
      (reduce + (map rperms subvecs)))))

(defn subvecs [x]
  (reductions conj [] (reverse x )))

(defn perms [x]
  (count (remove false?
                 (map (fn [y] (< y (last x))) (butlast x)))))

(defn permutations
  "Returns all permutations of c."
  [c]
  (map vec (lazy-seq
            (if (next c)
              (for [head c
                    tail (permutations (disj c head))]
                (cons head tail))
              [c]))))

(def three (rand-squarevec 3 5))
(def two   (rand-squarevec 2 5))

  ;; 0. [ 3 1 3 ]
  ;; 1. [ 3 4 0 ]
  ;; 2. [ 1 4 3 ]

(defn indexvec [c]
  (for [x   (range (count c))]
    [x (c x)]))


(defn step1
  [coll index]
  (for [x (map (fn [c] (get-in coll c)) index)]
    x))

(defn sign
  [i]
  (int (Math/pow -1 (Nperms i))))

(defn step2
  [coll]
  (map (fn [c]
         (step1 coll c))
       (map indexvec (permutations (set (range (count coll)))))))


(def string "1 2 3 4")


(let [matrix [[-2.3 4 3] [3 -5 7] [8.5 9 2]]
      signs  (map sign (permutations (set (range (count matrix)))))
      terms  (map (fn [c] (reduce * c)) ( step2 matrix ))
      sterms (mapv * signs terms)
      determinant (reduce + sterms)]
  {:matrix matrix :signs signs :terms terms :sterms sterms :determinant determinant})

(defn determinant
  [coll]
  (reduce +
          (mapv * (map sign (permutations (set (range (count coll)))))
                (map (fn [c] (reduce * c)) (step2 coll)))))
