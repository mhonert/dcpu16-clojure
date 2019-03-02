(in-ns 'dcpu16.compiler.core)

(defn check-operand [operation word code op-num stack-offset op2-literal]
  "returns [rem-code corrected-code new-stack-offset op-literal]"
  (if-bind (= 1 op-num) [opcode (bit-and 0x1f (bit-shift-right word 5)) (bit-shift-right word 10)]
    (cond
      ; SP register
      (= opcode 0x1b)
      (cond 
        (= :ADD operation)
        [code nil (- stack-offset op2-literal)]
        (= :SUB operation)
        [code nil (+ stack-offset op2-literal)]
        :else
        [code nil stack-offset])
      ; any other register
      (or (in-range? opcode 0x00 0x07)
          (= opcode 0x19)
          (in-range? opcode 0x1c 0x1d))
      [code nil stack-offset]
      ; PUSH/POP
      (= opcode 0x18)
      [code nil (if (= op-num 1) 
                  (inc stack-offset) ; PUSH => SP decreases => offset must be increased
                  (dec stack-offset) ; POP  => SP increases => offset must be decreased
                  )]
      ; [SP + next word]
      (= opcode 0x1a)
      [(next code) [(bit-and (+ stack-offset (first code)) 0xFFFF)] stack-offset] ; add the stack offset
      ; [register]
      (in-range? opcode 0x08 0x0f)
      [code nil stack-offset]
      ; [next word + register]
      (in-range? opcode 0x10 0x17)
      [(next code) [(first code)] stack-offset]
      ; [next word]
      (= opcode 0x1e)
      [(next code) [(first code)] stack-offset]
      ; next word (literal)
      (= opcode 0x1f)
      [(next code) [(first code)] stack-offset (first code)]
      :else ; literal value 0xffff-0x1e (-1 to 30)
      [code nil stack-offset (dec (- opcode 0x20))])))
      
(defn check-basic-opcode [operation word code stack-offset]
  "returns [rem-code corrected code new-stack-offset]"
  (let [[rest1 corrected-code1 stack-offset op2-literal] (check-operand (keyword operation) word code 2 stack-offset 0)
        [rest2 corrected-code2 new-stack-offset _] (check-operand (keyword operation) word rest1 1 stack-offset op2-literal)]
    [rest2 (concat [word] corrected-code2 corrected-code1) new-stack-offset]))

(defn check-non-basic-opcode [word code stack-offset]
  "returns [rem-code corrected-code new-stack-offset]"
  (let [opcode (bit-and 0x1f (bit-shift-right word 5))
        operation (non-basic-opcodes opcode)]
    (if operation
      (let [[rest1 corrected-code new-stack-offset] (check-operand (keyword operation) word code 2 stack-offset 0)]
        [rest1 (concat [word] corrected-code) new-stack-offset])
      (compiler-error (str "Unexpected non-basic opcode: " opcode)))))

(defn check-opcode [[word & code] stack-offset]
  "returns [rem-code corrected-code new-stack-offset]"
  (if (or (vector? word) (keyword? word)) ; label?
    [code [word] stack-offset]
    (let [opcode (bit-and 0x001F word)]
      (cond 
        ;non-basic-opcode
        (= 0 opcode) 
        (check-non-basic-opcode word code stack-offset)
        ;basic opcode
        (opcodes opcode)
        (check-basic-opcode (opcodes opcode) word code stack-offset)
        :else
        (compiler-error (str "Unexpected opcode: " opcode))))))

(defn correct-sp-refs
  "Corrects all operands, where [SP + literal] is used to access a local variable/parameter.
   The correction is done by increasing/decreasing the literal each time the register SP changes
   (e.g. due to PUSH, POP, etc.)
  Returns: [corrected-code]"
  [src-code]
  (loop [code src-code
         result []
         stack-offset 0]
    (if code
      (let [[rem-code corrected-code new-stack-ofset] (check-opcode code stack-offset)]
        (recur rem-code 
               (concat result corrected-code)
               new-stack-ofset))
      result)))
  