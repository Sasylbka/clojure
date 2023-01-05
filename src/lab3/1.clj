(ns lab3.1)

(defn split [coll n]
  (if (empty? coll)
    ()
    (concat (list (take n coll)) (split (drop n coll) n))
    )
  )

(defn parallel-filter-batch [pred coll batch-size]
  (->> (split coll batch-size)
       (map #(future (doall (filter pred %))))
       (doall)
       (mapcat deref)
       )
  )

(defn parallel-filter-threads [pred coll threads]
  (let [size (count coll)]
    (if (== (mod size threads) 0)
      (parallel-filter-batch pred coll (quot size threads))
      (parallel-filter-batch pred coll (+ 1 (quot size threads)))
      )
    )
  )

(defn parallel-filter
  ([pred coll] (parallel-filter-threads pred coll (.availableProcessors (Runtime/getRuntime))))
  ([pred coll threads] (parallel-filter-threads pred coll threads))
  )

(def test-func (range 200))

(defn condition [x]
  (Thread/sleep 10)
  (= 0 (mod x 15))
  )

(time (prn (filter condition test-func)))
(time (prn (parallel-filter condition test-func 2)))
(time (prn (parallel-filter condition test-func)))
(time (prn (parallel-filter condition test-func 10)))


(def collection
  '((2 :B 5 "$HELP") (1 "ABCDE" 10 :A 6) (0) (4 5 3 2) (4 ("4" 4) 4 4) () ((3 "7")) (3 2 ()) ((1 "qwe" (:A "55")) 2 1) () ((:A ("11" "22")) 3) (() ()))
  )

(defn collection-condition [x]
  (Thread/sleep 100)
  (even? (count x))
  )

(time (prn (filter collection-condition collection)))
(time (prn (parallel-filter collection-condition collection 2)))
(time (prn (parallel-filter collection-condition collection)))
(time (prn (parallel-filter collection-condition collection 10)))