(ns lab1.2)
(defn new-word [alph oldWord acc]
  (cond
    (empty? alph) acc
    true (let [first-letter (first alph)
          rest-letter (rest alph)
          temp (cons first-letter oldWord)]
      (if (= first-letter (first oldWord))
        (recur rest-letter oldWord acc)
        (recur rest-letter oldWord (cons temp acc))
        ))))

(defn gen-words
  ([alph n] (gen-words alph n (new-word alph "" "") "" ""))
  ([alph n oldWords new-words acc]
   (let [first-word (first oldWords)
         rest-words (rest oldWords)
         temp (new-word alph first-word "")]
     (cond
       (= n 1) acc
       true (if (empty? rest-words)
              (recur alph (- n 1) (concat new-words temp) "" (concat new-words temp))
              (recur alph n rest-words (concat new-words temp) (concat new-words temp)))))))


(defn start [alph n]
  (gen-words alph n))