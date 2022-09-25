(ns lab1.1)

(defn new-word [alph oldWord]
  (if (empty? alph)
    (list)
    (let [first-letter (first alph)
          rest-letter (rest alph)
          temp (cons first-letter oldWord)]
      (if (= first-letter (first oldWord))
        (new-word rest-letter oldWord)
        (cons temp (new-word rest-letter oldWord))
        ))))

(defn gen-words
  ([alph n] (gen-words alph n (new-word alph "") ""))
  ([alph n oldWords new-words]
   (let [first-word (first oldWords)
         rest-words (rest oldWords)
         temp (new-word alph first-word)]
     (cond
       (= n 1) oldWords
       true (if (empty? rest-words)
              (gen-words alph (- n 1) (concat new-words temp) "")
              (gen-words alph n rest-words (concat new-words temp)))))))


(defn start [alph n]
  (gen-words alph n))
