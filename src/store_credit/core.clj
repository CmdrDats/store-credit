(ns store_credit.core)

(defn read-input
  "Reads the input file and constructs cases out of it."
  [filename]
  (let [contents (.split (slurp filename) "\n")
        casecount (first contents)
        cases (partition 3 (rest contents))]
    (for [case cases]
      {:credit (Integer/parseInt (first case))
       :itemcount (Integer/parseInt (second case))
       :items (map #(Integer/parseInt %) (seq (.split (nth case 2) "[ ]")))})))

(defn solve-case
  "Solves a case, finds the two items numbers required for fulfilling the credit exactly"
  [{:keys [credit items]}]
  (println "Solving case....")
  (loop [item (first items) others (rest items) check 0 itm 0]
    (let [next (nth others check :not-found)]
      (if (> item credit)
        (recur (first others) (rest others) 0 (inc itm))
        (if (not (nil? item))
          (if (= next :not-found)
            (recur (first others) (rest others) 0 (inc itm))
            (if (= credit (+ item next))
              [(inc itm) (inc (+ itm check 1))]
              (recur item others (inc check) itm))
            ))))
    
    ))

(defn solve
  "Solve from input file and write solutions to output file"
  [fromfile tofile]
  (let [cases (read-input fromfile)
        solutions (map solve-case cases)
        result (map #(str "Case #" (inc %2) ": " (first %1) " " (second %1) "\r\n") solutions (range))]
    (spit tofile (apply str result)))
  )