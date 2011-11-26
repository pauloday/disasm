(ns disasm.lookups
  ( :use clojure.contrib.math))

(defn get-bits
  "Gets the bits from s to e in a number or if only s is given returns bit s
For example: (get-bits 0b101101 1 3) returns 110
             (get-bits 0b101101 1) returns 1"
  ([n s]
     (let [mask (expt 2 s)]
       (bit-shift-right (bit-and mask n) s)))

  ([n s e] (let [mask (apply bit-or (for [x (range s (+ 1 e))]
                                      (expt 2 x)))]
             (bit-shift-right (bit-and mask n) s))))

(defn match
  "Matches a number with a simple pattern string
Example: (match 2r110 1x0) returns true (1x0 is a string)
if there are multiple ns and strs it returns the and of all of them"
  ([n str & nstrs]
     (reduce (fn [x y] (and x y))
             [(match n str)
              (apply #'match nstrs)]))
  ([n str]
      (let [string (reverse str)]
        (loop [s (last string) c (- (count string) 1)]
          (if (or (= \x s)
                  (= (Character/digit s 2) (get-bits n c)))
            (if (= c 0)
              true
              (recur (nth string (- c 1)) (- c 1))))))))

(def
  ^{:doc
    "returns a string to be appended to the end of the opname"}
  conds
  {2r0000 "eq"
   2r0001 "ne"
   2r0010 "cs"
   2r0011 "cc"
   2r0100 "mi"
   2r0101 "pl"
   2r0110 "vs"
   2r0111 "vc"
   2r1000 "hi"
   2r1001 "ls"
   2r1010 "ge"
   2r1011 "lt"
   2r1100 "gt"
   2r1101 "le"
   2r1110 ""})

(defn cat [op op1 op2]
  (if (= op 0)
    (cond
     (and (not (match op1 "10xx0"))
          (match op2 "xxx0"))
     "A5-5"
     (and (not (match op1 "10xx0"))
          (match op2 "0xx1"))
     "A5-7"
     (match op1 "10xx0" op2 "0xxx")
     "A5-18"
     (match op1 "10xx0" op2 "1xx0")
     "A5-13"
     (match op1 "0xxxx" op2 "1001")
     "A5-12"
     (match op1 "1xxxx" op2 "1001")
     "A5-16"
     (and (not (match op1 "0xx1x"))
          (match op2 "1011"))
     "A5-14"
     (not (match op1 "0xx1x"))
     "A5-14"
     (match op1 "0xx1x")
     "A5-15")
    (cond
     (not (match op1 "10xx0"))
     "A5-8"
     (match op1 "10x10")
     "A5-17")))

;;; Data processing (register) instructions
(defn A5-5 [op1 op2 op3]
  (condp match op1
   "0000x" "AND"
   "0001x" "EOR"
   "0010x" "SUB"
   "0011x" "RSB"
   "0100x" "ADD"
   "0101x" "ADC"
   "0110x" "SBC"
   "0111x" "RSC"
   "10001" "TST"
   "10011" "TEQ"
   "10101" "CMP"
   "10111" "CMN"
   "1100x" "ORR")
  (if (match op1 "1101x")
    (cond
     (match op2 "00000" op3 "00")
     "MOV"
     (and (not (match op2 "000000"))
          (match op3 "00"))
     "LSL"
     (match op3 "01")
     "LSR"
     (match op3 "10")
     "ASR"
     (match op2 "00000" op3 "11")
     "RRX"
     (and (not (match op2 "000000"))
          (match op3 "11"))
     "ROR")))

;;; Data processing (register-shifted-register) instructions
(defn A5-7 [op1 op2]
  (condp match op1
    "2r1110x" "BIC"
    "2r1111x" "MVN"
    (A5-5 op1 1 op2)))

;;; this may be extended for some other instructions later, this is
;;; simplified, MSR is actually a bunch of different fns for
;;; application, system level
;;; Miscellaneous instructions
(defn A5-18 [op op1 op2]
  (condp match op
    "x0" "MRS"
    "01" "MSR"
    "11" "MSR"))

;;; Halfword multiply-accumulate instructions
(defn A5-13 [op op1]
  (condp match op1
    "00" :SML
    "01" :SML
    "10" :SMU
    "01" :SMU))

;;; Multiply and multiply-accumulate instructions
(defn A5-12 [op]
  (condp match op
    "000x" "MUL"
    "001x" "MLA"
    "0100" "UMAAL"
    "0110" "MLS"
    "100x" "UMULL"
    "101x" "UMLAL"
    "110x" "SMULL"
    "111x" "SMLAL"))

;;; This may need to be fixed as well, depending on what version
;;; Synchronization primitives
(defn A5-16 [op]
  (when (match op "0x00") :SWP))

;;; May need update
;;; Extra load/store instructions
(defn A5-14 [op1 op2 rn]
  (condp match op2
    "01" (condp match op1
           "xx0x0" "STRH"
           "xx0x1" "LDRH"
           "xx1x0" "STRH"
           "xx1x1" "LDRH")
    "10" (condp match op1
           "xx0x0" "LDRD"
           "xx0x1" "LDRSB"
           "xx1x0" "LDRD"
           "xx1x1" "LDRSB")
    "11" (condp match op1
           "xx0x0" "STRD"
           "xx0x1" "LDRSH"
           "xx1x0" "STRD"
           "xx1x1" "LDRSH")))

;;; Extra load/store instructions (unpriveledged)
(defn A5-15 [op2 op rt]
  (condp match op2
    "01" (if (= op 1)
           "STRHT"
           "LDRHT")
    "10" (when (= op 1) "LDRSBT")
    "11" (when (- op 1) "LDRSHT")))

;;; Data-rocessing (immediate) instructions
(defn A5-8 [op rn]
  (A5-5 op 0000 00)
  (condp match op
    "1110x" "BIC"
    "1111x" "MVN"))