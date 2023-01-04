(ns lab2.2)
(defn simpleShift [x] (+ x 5))
(defn sqr [x] (* x x))

(defn area [fun start end]
  (* (- end start) (* 0.5 (+ (fun start) (fun end)))))

(defn WithDelta [fun start end delta]
  (* (* (- end start) delta)
     (* 0.5 (+ (fun (* delta start)) (fun (* delta end))))))

(defn integrateValues [fun delta]
  (reductions + (map (fn [x] (WithDelta fun x (+ x delta) delta))
                     (iterate (fn [x] (+ x delta)) 0))))

(defn integrateTo [fun end delta history]
  (let [s (int (/ end delta))]
    (+ (area fun (- end (- end (* delta s))) end)
       (nth history s))))

(defn seqIntegrate [fun]
  (let [delta 1, history (integrateValues fun delta)]
    (fn [x] (integrateTo fun x delta history))))

(let [simpleShiftIntegral (seqIntegrate simpleShift), sqrIntegral (seqIntegrate sqr)]
  (time (simpleShiftIntegral 100))
  (time (simpleShiftIntegral 90))
  (println "--------------")
  (time (sqrIntegral 100))
  (time (sqrIntegral 90)))