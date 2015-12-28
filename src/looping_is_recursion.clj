(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k]
                 (if (== k 1)
                   acc
                   (recur (* acc base) (dec k))))]
    (if (== exp 0)
      1
      (helper base exp))))

 (defn last-element [a-seq]
   (let [helper (fn [acc seq]
                  (if (empty? seq)
                    acc
                    (recur (first seq) (rest seq))))]
     (helper nil a-seq)))

(defn seq= [a-seq b-seq]
  (let [helper (fn [seq1 seq2]
                 (cond
                  (or (empty? seq1) (empty? seq2)) (and (empty? seq1) (empty? seq2))
                  (not (== (first seq1) (first seq2))) false
                  :else (recur (rest seq1) (rest seq2))))]
    (helper a-seq b-seq)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
     (empty? seq) nil
     (pred (first seq)) index
     :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [seq a-seq
         sum 0
         nbr 0]
    (if (empty? seq)
      (/ sum nbr)
      (recur (rest seq) (+ sum (first seq)) (inc nbr)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
  (loop [seq a-seq
         a-set #{}]
    (if (empty? seq)
      a-set
      (recur (rest seq) (toggle a-set (first seq)))))))

(defn fast-fibo [n]
  (loop [i 2
         fibo 1
         previous 1]
    (cond
     (< n 2) n
     (== i n) fibo
     :else (recur (inc i) (+ fibo previous) fibo))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         cut-set #{}
         cut []]
    (if (or (empty? seq) (contains? cut-set (first seq)))
      cut
      (recur (rest seq) (conj cut-set (first seq)) (conj cut (first seq))))))

