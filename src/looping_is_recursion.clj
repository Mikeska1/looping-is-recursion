(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [eka (first a-seq)
        loput (rest a-seq)]
    (cond
     (empty? a-seq) nil
     (empty? loput) eka
     :else (recur loput))))

(defn seq= [seq1 seq2]
  (let [e1 (empty? seq1)
        e2 (empty? seq2)]
    (cond
     (and e1 e2) true
     e1 false
     e2 false
     (= (first seq1) (first seq2))
       (recur (rest seq1) (rest seq2))
     :else false
   )))

(defn find-first-index [pred a-seq]
  (loop [i 0
         sekvenssi a-seq]
    (cond
     (empty? sekvenssi) nil
     (pred (first sekvenssi)) i
     :else (recur (inc i) (rest sekvenssi)))))

(defn avg [a-seq]
  (loop [sekvenssi a-seq
         summa 0
         elementit 0]
    (cond
     (empty? sekvenssi) 0
     (empty? (rest sekvenssi))
       (/ (+ summa (first sekvenssi))
          (inc elementit))
     :else (recur (rest sekvenssi)
                  (+ summa (first sekvenssi))
                  (inc elementit)))))

(defn parity [a-seq]
  (loop [sekvenssi a-seq
         joukko #{}]
    (cond
     (empty? sekvenssi) joukko
     :else (recur (rest sekvenssi)
                  (let [eka (first sekvenssi)]
                    (if (contains? joukko eka)
                        (disj joukko eka)
                        (conj joukko eka)))))))

(defn fast-fibo [n]
  (loop [i 1
         f1 1
         f0 0]
    (cond
     (zero? n) 0
     (= n 1) 1
     (= n i) f1
     :else (recur (inc i) (+ f1 f0) f1))))

(defn cut-at-repetition [a-seq]
  (loop [joukko #{}
         sekvenssi a-seq
         eka (first a-seq)
         loput (rest a-seq)
         tulos '()]
    (cond
     (empty? sekvenssi) (reverse tulos)
     (contains? joukko eka) (reverse tulos)
     :else (recur (conj joukko eka)
                  loput
                  (first loput)
                  (rest loput)
                  (cons eka tulos)))))

