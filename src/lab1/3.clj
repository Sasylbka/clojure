(ns lab1.3)

(defn my-map [func arg]
  (reduce (fn [temp i] (conj temp (func i)))
          (vec '())
          arg))

(defn my-filter [func arg]
  (reduce (fn [temp i]
            (if (func i) (conj temp i) temp))
          (vec '()) arg))