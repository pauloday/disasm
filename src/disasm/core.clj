(ns disasm.core
  (:use disasm.lookups))

(defn get-info [inst rep val table start & end]
  (let [vl (get-bits inst start end)]
     (assoc rep val (table vl))))

(defn get-cond [inst rep]
  (get-info inst rep :cond conds 28 31))

(defn get-op-cat1 [inst rep]
  (let [op (get-bits 25)
        op1 (get-bits 20 24)
        op2 (get-bits 4 7)]
    (assoc rep :cat (cat '(op op1 op2)))))

(defn get-op-name [inst rep])