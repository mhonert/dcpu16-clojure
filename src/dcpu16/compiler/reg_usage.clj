(in-ns 'dcpu16.compiler.core)

(defn reg-usage-operand [operation word code op-num]
  "returns [rem-code used-regs]"
  (if-bind (= 1 op-num) [opcode (bit-and 0x1f (bit-shift-right word 5)) (bit-shift-right word 10)]
    (cond
      ; Registers C, X, Y or Z
      (in-range? opcode 0x02 0x05)
      [code [(keyword (regopcodes opcode))]]
      ; any other register
      (or (in-range? opcode 0x00 0x01)
          (in-range? opcode 0x06 0x07)
          (= opcode 0x19)
          (in-range? opcode 0x1b 0x1d))
      [code nil]
      ; PUSH/POP
      (= opcode 0x18)
      [code nil]
      ; [SP + next word]
      (= opcode 0x1a)
      [(next code) nil]
      ; [register]
      (in-range? opcode 0x08 0x0f)
      [code nil]
      ; [next word + register]
      (in-range? opcode 0x10 0x17)
      [(next code) nil]
      ; [next word]
      (= opcode 0x1e)
      [(next code) nil]
      ; next word (literal)
      (= opcode 0x1f)
      [(next code) nil]
      :else ; literal value 0xffff-0x1e (-1 to 30)
      [code nil])))
      
(defn reg-usage-basic-opcode [operation word code]
  "returns [rem-code used-regs]"
  (let [[rest1 _] (reg-usage-operand (keyword operation) word code 2)
        [rest2 regs] (reg-usage-operand (keyword operation) word rest1 1)]
    [rest2 regs]))

(defn reg-usage-non-basic-opcode [word code]
  "returns [rem-code used-regs]"
  (let [opcode (bit-and 0x1f (bit-shift-right word 5))
        operation (non-basic-opcodes opcode)]
    (if operation
      (let [[rest1 regs] (reg-usage-operand (keyword operation) word code 2)]
        [rest1 nil])
      (compiler-error (str "Unexpected non-basic opcode: " opcode)))))

(defn reg-usage-opcode [[word & code]]
  "returns [rem-code used-regs]"
  (if (or (vector? word) (keyword? word)) ; label?
    [code nil]
    (let [opcode (bit-and 0x001F word)]
      (cond 
        ;non-basic-opcode
        (= 0 opcode) 
        (reg-usage-non-basic-opcode word code )
        ;basic opcode
        (opcodes opcode)
        (reg-usage-basic-opcode (opcodes opcode) word code)
        :else
        (compiler-error (str "Unexpected opcode: " opcode))))))

(defn determine-reg-usage
  "Scans the provided code and returns a vector of registers that get modified within the code
   (only checks registers C, X, Y and Z because they may be used by a calling function within an expression)"
  [src-code]
  (loop [code src-code
         regs #{}]
    (if code
      (let [[rem-code new-regs] (reg-usage-opcode code)]
        (recur rem-code 
               (into regs new-regs)))
      (vec regs))))

