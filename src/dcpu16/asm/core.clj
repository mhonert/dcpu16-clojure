(ns dcpu16.asm.core (:use [dcpu16.util]))

(load "defs")
(load "emit")
(load "disasm")

(defn syntax-error [msg]
  (throw (Exception. msg)))

(defn read-mem-operand [mem-ops]
  "Allowed combinations:
   [REG], [:LABEL], [LITERAL], [:LABEL REG], [:LABEL REG LITERAL]"
  (let [{:keys [reg-vec lit-vec lab-vec]} (group-by #(cond
                                           (regkeywords %) :reg-vec
                                           (number? %) :lit-vec
                                           (keyword? %) :lab-vec
                                           :else (syntax-error (str "Unexpected operand part: " %)))
                                        mem-ops)
        reg (first reg-vec)
        lit (if lit-vec (reduce + lit-vec) nil)
        lab (first lab-vec)]
    (assert (<= (count reg-vec) 1) (str "There is more than one registers provided in a MEM operand: " reg-vec))
    (assert (<= (count lab-vec) 1) (str "There is more than one label provided in a MEM operand: " lab-vec))    
    (cond
      reg (cond (and lit lab) [(memreglitop reg lit) lab]
                lab [(memreglitop reg 0) lab]
                lit [(memreglitop reg lit)]
                :else [(memregop reg)])
      (and lit lab) [(memlitop lit) lab]
      lit [(memlitop lit)]
      lab [(memlitop 0) lab])))

(defn read-lit-label-operand [lit-ops]
  {:pre [(= 2 (count lit-ops))]}
  (if-bind (number? (first lit-ops)) [lit (first lit-ops) (second lit-ops)
                                      lab (second lit-ops) (first lit-ops)]
    (assert (and (number? lit) (keyword? lab)) (str "Expects a literal and a label, but got :" lit " and " lab))
    [(litop lit) lab]))

(defn read-1-operand [op]
  "Reads 1 operand from the given list of tokens"
  (cond
    (regkeywords op) [(regop (regkeywords op))]   ; register
    (vector? op) (read-mem-operand op)            ; memory access
    (keyword? op) [(litop 0 :no-optimization) op] ; label
    (number? op) [(litop op)]                     ; literal
    (set? op) (read-lit-label-operand (seq op))   ; literal + label
    :else (syntax-error (str "Unexpected operand: " (type op) ": " op))))

(defn read-2-operands [op1 op2]
  "Reads 2 operands from the given list of tokens"
  [(read-1-operand op1) (read-1-operand op2)])

(defn read-data [data]
  "Reads a data definition.
   Syntax: DAT [data]
   Example: DAT [1 2 3 4 \"Test\""
  (if (vector? data)
    (vec 
      (mapcat 
        #(cond 
           ; String?
           (string? %)
           (map int %)
           ; Label?
           (symbol? %)
           [[0 (keyword %)] 0]
           ; Number?
           (number? %)
            (cond 
              ; Fixed point number?
              (float? %)
              (do
                (assert (and (>= (int %) -32768) (<= (int %) 32767)) "Fixed point number is < -32768 or > 32767")
                (if-bind (< % 0) [v (+ % 65536) %]
                         [(int v) (Math/abs (int (* 65536 (- (int %) %))))]))
              ; 32 Bit?
              (or (< % -32768) (> % 65535))
              (do
                (assert (and (>= % -2147483648) (<= % 4294967295)) "32-Bit number is < -2147483648 or > 4294967295")
                (if-bind (< % 0) [v (+ % 4294967296) %]
                         [(bit-and v 0x0000FFFF) (bit-shift-right v 16)]))
              ; 16 Bit
              :else
              (if-bind (< % 0) [v (+ % 65536) %]
                       [v]))
            :else
            (syntax-error (str "Unexpected data element: " %)))
         data))
    (syntax-error (str "Data block expects a vector of data elements, instead of: " data))))

(defn asm* [operation & operands]
  "Emit an assembler operation (unoptimized)"
  (if (opkeywords operation) ; Operation with 2 operands?
    (let [[[op1 label1] [op2 label2]] (read-2-operands (first operands) (second operands))]
      (assert (= 2 (count operands)) (str "Operation " operation " expects 2 operands, but got :" operands))
      (cond
        (and label1 label2)
        (concat [[(dec (count op1)) label1] [(+ (dec (count op1)) (dec (count op2))) label2]] (demit operation op1 op2))
        label1
        (concat [[(dec (count op1)) label1]] (demit operation op1 op2))
        label2
        (concat [[(+ (dec (count op1)) (dec (count op2))) label2]] (demit operation op1 op2))
        :else
        (vec (demit operation op1 op2))))
    ; Operation with 1/0 operand?
    (if (non-basic-opkeywords operation)
      (let [[op1 label1] (read-1-operand (first operands))]
        (assert (= 1 (count operands)) (str "Operation " operation " expects 1 operand, but got :" operands))
        (if label1
          (concat [[(dec (count op1)) label1]] (demit operation op1))
          (vec (demit operation op1))))
      (cond 
        ; Label?
        (keyword? operation)
        [operation]
        ; Data
        (vector? operation)
        (read-data operation)
        :else
        (syntax-error (str "Unknown operation: " operation " / operands: " operands ))))))

(defmulti asm
  "Emit an assembler operation and perform some simple optimizations, if possible"
  (fn [operation & operands] operation))

(defmethod asm :MUL [operation op1 op2]
  (if (number? op2)
    (cond
      (= op2 (bit-shift-left 1 (- 31 (Integer/numberOfLeadingZeros op2))))
      (asm* :SHL op1, (- 31 (Integer/numberOfLeadingZeros op2)))
      :else
      (asm* operation op1 op2))
    (asm* operation op1 op2)))

(defmethod asm :MLI [operation op1 op2]
  (if (number? op2)
    (cond
      (= op2 (bit-shift-left 1 (- 31 (Integer/numberOfLeadingZeros op2))))
      (asm* :SHL op1, (- 31 (Integer/numberOfLeadingZeros op2)))
      :else
      (asm* operation op1 op2))
    (asm* operation op1 op2)))

(defmethod asm :DIV [operation op1 op2]
  (if (number? op2)
    (cond
      (= op2 (bit-shift-left 1 (- 31 (Integer/numberOfLeadingZeros op2))))
      (asm* :SHR op1, (- 31 (Integer/numberOfLeadingZeros op2)))
      :else
      (asm* operation op1 op2))
    (asm* operation op1 op2)))

(defmethod asm :DVI [operation op1 op2]
  (if (number? op2)
    (cond
      (= op2 (bit-shift-left 1 (- 31 (Integer/numberOfLeadingZeros op2))))
      (asm* :ASR op1, (- 31 (Integer/numberOfLeadingZeros op2)))
      :else
      (asm* operation op1 op2))
    (asm* operation op1 op2)))

(defmethod asm :MOD [operation op1 op2]
  (if (number? op2)
    (cond
      (= op2 (bit-shift-left 1 (- 31 (Integer/numberOfLeadingZeros op2))))
      (asm* :AND op1, (- op2 1))
      :else
      (asm* operation op1 op2))
    (asm* operation op1 op2)))


(defmethod asm :BOR [operation op1 op2]
  (if (and (number? op2) (= op2 0))
    nil
    (asm* operation op1 op2)))

(defmethod asm :XOR [operation op1 op2]
  (if (and (number? op2) (= op2 0))
    nil
    (asm* operation op1 op2)))

(defmethod asm :default [operation & operands]  
  (apply asm* operation operands))

(load "labels")
