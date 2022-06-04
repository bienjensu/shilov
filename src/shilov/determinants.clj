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

(defn Npermutations [c]
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

(defn permutations [c]
  "Returns all permutations of c."
  (map vec (lazy-seq
            (if (next c)
              (for [head c
                    tail (permutations (disj c head))]
                (cons head tail))
              [c]))))

(def three (rand-squarevec 3 5))

  ;; 0. [ 3 1 3 ]
  ;; 1. [ 3 4 0 ]
  ;; 2. [ 1 4 3 ]

(defn indexvec [c]
  (for [x   (range (count c))
        y   c]
    [x y]))

(defn determinant [m]
  "Returns the determinant of square matrix m."
  (map
   (fn [x] (get-in m x))
   (indexvec '(2 1 0))))

