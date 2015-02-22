(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (cond
                  (= 0 exp)  acc
                  :else
                  (recur (* acc base) base (dec exp))))]
        (helper 1 base exp)))

(defn last-element [a-seq]
  (let [fst (first a-seq)]
    (if (empty? (rest a-seq))
      fst
      (recur (rest a-seq)))))

(defn seq= [seq1 seq2]
  (let [head1 (first seq1)
        head2 (first seq2)
        rest1 (rest seq1)
        rest2 (rest seq2)]
    (cond (not (= head1 head2)) false
          (and (empty? rest1) (empty? rest2)) true
          (or (and (empty? rest1) (not (empty? rest2))) (and (not (empty? rest1)) (empty? rest2))) false
          :else (recur rest1 rest2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         p pred
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) i
     :else (recur (+ 1 i) p (rest s)))))


(defn avg [a-seq]
  (loop [acc 0
         i 1
         s a-seq]
    (cond
     (nil? (first s)) (/ acc i)
     (empty? (rest s)) (/ (+ acc (first s)) i)
      :else (recur (+ acc (first s)) (+ 1 i) (rest s)))))

(defn toggle [a-set e]
  (if(contains? a-set e)
    (disj a-set e)
    (conj a-set e)))

(toggle #{1 2 3} 2)

(defn parity [a-seq]
  (loop [pairs #{}
         s a-seq]
    (let [fst (first s)]
      (if (nil? fst)
      pairs
      (recur (toggle pairs fst) (rest s))))))

(defn fast-fibo [n]
  (loop [pp 0
         p 1
         i 0]
    (if (= i n) pp
      (recur p (+ p pp) (+ i 1)))))

(defn cut-at-repetition [a-seq]
  (let [fst (first a-seq)]
        (concat [fst] (take-while (fn [e] (not (= e fst))) (rest a-seq)))))



