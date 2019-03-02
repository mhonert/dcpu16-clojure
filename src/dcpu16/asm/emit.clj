(in-ns 'dcpu16.asm.core)

(defn regop [reg]
  "Emits operand: register"
  (let [regvalue (regs reg)]
    (assert regvalue (str "Unknown register: " reg))
    [regvalue]))

(defn memregop [reg]
  "Emits operand: [register]"
  (let [regvalue (regs reg)]
    (assert regvalue (str "Unknown register: " reg))
    (assert (allowed-mem-regs reg) (str "Register '" reg "' cannot be used for memory access!"))
    (if (= reg :SP)
      [0x19]
      [(+ 0x08 regvalue)])))

(defn memreglitop [reg value]
  "Emits operand: [next word + register]"
  (do
    (assert (and (>= value 0) (< value 65536)) (str "Literal operand is not a 16-bit value: " value))
    (assert (allowed-mem-regs reg) (str "Register '" reg "' cannot be used for memory access!"))
    (let [regvalue (regs reg)]
      (assert regvalue (str "Unknown register: " reg))
      (if (= reg :SP)
        [0x1a value]
        [(+ 0x10 regvalue) value]))))
  
(defn memlitop [value]
  "Emits operand: [next word]"
  (do
    (assert (and (>= value 0) (< value 65536)) (str "Literal operand is not a 16-bit value: " value))
    [0x1e value]))

(defn litop [value & flags]
  "Emits operand: literal; optional flags
   - :no-optimization -> always put the value into a separate word"
  (do 
    (assert (and (>= value -32768) (< value 65536)) (str "Literal operand is not a 16-bit value: " value))
    (let [cvalue (if (< value 0) 
                 (+ value 65536)
                 value)] 
      (if (or (and (> cvalue 0x1e) (not= cvalue 0xffff)) (= (first flags) :no-optimization))
        [0x1f cvalue] ; next word (literal)
        [(if (= cvalue 0xffff ) 0x20 (+ cvalue 0x21))])))) ; literal value 0xffff-0x1e (-1 to 30)

(defn demit 
  ([operation op1 op2]
    "Emits a basic opcode for operation with two operands"
    (let [operation-code (operations operation)]
      (assert operation-code (str "Unknown operation: " operation))
      (concat [(bit-or operation-code (bit-shift-left (first op1) 5) (bit-shift-left (first op2) 10))]
              (rest op2)
              (rest op1))))
  ([operation op]
    "Emits a non-basic opcode for operations with one operand"
    (let [operation-code (non-basic-operations operation)]
      (assert operation-code (str "Unknown operation: " operation))
      (concat [(bit-or  (bit-shift-left operation-code 5) (bit-shift-left (first op) 10))]
              (rest op)))))

 
(defn code-to-hexstr [code]
  (map #(format "%4h" %) code))
              
(defn dump-mem [mem]
  "Prints a hexadecimal representation of the mem argument"
  (let [lines (partition 8 8 [0 0 0 0 0 0 0 0] mem)
        addrs (map #(* 8 %) (range (count lines)))]
    (doseq [[addr line] (map list addrs lines)]
      (when (not (every? zero? line))
        (print (str (format "%04x" addr) ": "))
        (doseq [v line] (print (format "%04x " v)))
        (print " |")
        (doseq [v line] (if (and (>= v 0x20) (<= v 0x7e))
                          (print (char v))
                          (print \.)))
        (println "|")))))


