(ns lab3.2)
(defn split-collection [n coll]
  (if (empty? coll)
    ()
    (lazy-seq (cons (take n coll) (split-collection n (drop n coll))))
    )
  )

(defn parallel-filter-batch [pred coll batch-size threads]
  (->> (split-collection batch-size coll)
       (split-collection threads)
       (map (fn [batch]
              (->> (map #(future (doall (filter pred %))) batch)
                   (doall threads)
                   (map deref)
                   )
              )
            )
       (apply concat)
       (apply concat)
       )
  )

(defn lazy-parallel-filter
  ([pred coll batch-size] (parallel-filter-batch pred coll batch-size (.availableProcessors (Runtime/getRuntime))))
  ([pred coll batch-size threads] (parallel-filter-batch pred coll batch-size threads))
  )


(def test-func (iterate inc 0))

(defn condition [x]
  (Thread/sleep 10)
  (= 0 (mod x 15))
  )

(time (prn (take 20 (filter condition test-func))))
(time (prn (take 20 (lazy-parallel-filter condition test-func 10 2))))
(time (prn (take 20 (lazy-parallel-filter condition test-func 10))))
(time (prn (take 20 (lazy-parallel-filter condition test-func 10 100))))


(def collection
  '((2 :B 5 "$HELP") (1 "ABCDE" 10 :A 6) (0) (4 5 3 2) (4 ("4" 4) 4 4) () ((3 "7")) (3 2 ()) ((1 "qwe" (:A "55")) 2 1) () ((:A ("11" "22")) 3) (() ()))
  )

(defn collection-condition [x]
  (Thread/sleep 100)
  (even? (count x))
  )

(time (prn (filter collection-condition collection)))
(time (prn (lazy-parallel-filter collection-condition collection 2 2)))
(time (prn (lazy-parallel-filter collection-condition collection 2)))
(time (prn (lazy-parallel-filter collection-condition collection 2 100)))