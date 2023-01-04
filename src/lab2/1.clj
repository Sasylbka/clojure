(ns lab2.1)

(defn simpleShift [x] (+ x 5))
(defn sqr [x] (* x x))

(defn area [f start end]
  (* (- end start) (* 0.5 (+ (f start) (f end)))))

(defn withDelta [fun  start  end  delta]
  (* (* (- end start) delta)
     (* 0.5 (+ (fun (* delta start)) (fun (* delta end))))))

(defn integralSteps [fun steps delta]
  (loop [n steps res 0]
    (if (= n 0) 0
                (if (= n 1)
                  (+ res (withDelta fun 0 1 delta))
                  (recur (- n 1) (+ res (withDelta fun (- n 1) n delta)))))))

(defn integrateTo [fun stepsFunction end delta]
  (let [s (int (/ end delta))]
    (+ (area fun (- end (- end (* delta s))) end) (stepsFunction fun s delta))))

(defn integrate [fun]
  (fn [x]
    (integrateTo fun integralSteps x 1)))

(defn memIntegrate [fun]
  (let [recurMemIntegrate (fn [recurFunc end]
                            (if (<= end 0)
                              0
                              (+ (area fun 0 (- end 1)) (recurFunc recurFunc (- end 1)))))]
    (partial recurMemIntegrate (memoize recurMemIntegrate))))

(let [simpleShiftIntegral (integrate simpleShift), sqrIntegral (integrate sqr),
      simpleShiftIntegralMem (memIntegrate simpleShift), sqrIntegralMem (memIntegrate sqr)]
  (time (simpleShiftIntegral 100))
  (time (simpleShiftIntegral 100))
  (println "---------------")
  (time (simpleShiftIntegralMem 70))
  (time (simpleShiftIntegralMem 70))
  (println "---------------")
  (time (sqrIntegral 100))
  (time (sqrIntegral 100))
  (println "----------------")
  (time (sqrIntegralMem 50))
  (time (sqrIntegralMem 50)))

(let [f (fn [x] (Thread/sleep 10) x) f-integral (memIntegrate f)]
  (time (f-integral 200))
  (time (f-integral 200)))