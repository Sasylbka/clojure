(ns lab1.4)

(defn gen-words [list alph]
           (reduce (fn [temp word] (concat word temp))'()
                   (map (fn [word]
                          (map (fn [x] (cons x word))
                               (filter (fn [letter]
                                         (not (= letter (first word))))
                                       alph)))
                        list)))

(defn looping [alph n]
  (reduce (fn [list i] (gen-words list alph))
          '(()) (range n))
  )

(defn start [alph]
 (looping alph (count alph)))

