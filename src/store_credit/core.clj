(ns store-credit.core
  (:gen-class))

;record for this problem
(defrecord state [credit items])

;numbers from file get read as chars, convert them to numbers
(defn to-numbers [line]
  (map read-string (re-seq #"[\d.]+" line)))

(def small (.split #"\n" (slurp "/Users/jbeaumon/code/clojure/store_credit/src/store_credit/storecredit.in")))
(def input (map to-numbers  small))
(def nTests (first input))
(def tests (rest input))

;create a delta list of (credit - itemPrice). 
;If we find the same number in the original and delta list, we've found a pair.
(defn make-delta [st]
  (map #(- (:credit st) %) (:items st)))

;search for pair between item and delta list
(defn find-pair [items delta credit]
  (let [i (first items)
        result (filter #(= i %) delta)]
    (if (not (empty? result))
      (conj result (- credit i))
      (recur (rest items) delta credit))))

;execute a single test
(defn find-items [st case-no]
  (let  [d (make-delta st)
        results (find-pair (:items st) d (:credit st))]
  (println (str "Case #" case-no ": (credit) " (:credit st) ": " results))))

;itterate through tests
(defn run [data case-no]
  (if (empty? data)
    nil
    (let [t (take 3 data)
          st (state. (ffirst t) (last t))
          dummy (find-items st case-no)]
      (recur (nthrest data 3) (inc case-no)))))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (run tests 1)
  )
