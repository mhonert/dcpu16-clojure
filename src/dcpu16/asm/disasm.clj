(in-ns 'dcpu16.asm.core)

(defn format-op [op]
  (if (number? op)
    (format "0x%04x" op)
    op))

(defn disasm-operand [word code op-num]
  "returns [rem-code result-str]"
  (if-bind (= 1 op-num) [opcode (bit-and 0x1f (bit-shift-right word 5)) (bit-shift-right word 10)]
    (cond
      ; register
      (or (in-range? opcode 0x00 0x07)
          (= opcode 0x19)
          (in-range? opcode 0x1b 0x1d))
      [code (regopcodes opcode)]
      ; PUSH/POP
      (= opcode 0x18)
      [code (if (= op-num 1) "PUSH" "POP")]
      ; [SP + next word]
      (= opcode 0x1a)
      [(next code) (str "[0x" (hex (first code)) " + SP]")]
      ; [register]
      (in-range? opcode 0x08 0x0f)
      [code (str \[ (regopcodes (- opcode 0x08)) \])]
      ; [next word + register]
      (in-range? opcode 0x10 0x17)
      [(next code) (str "[0x" (hex (first code)) " + " (regopcodes (- opcode 0x10)) \])]
      ; [next word]
      (= opcode 0x1e)
      [(next code) (str "[0x" (hex (first code)) \])]
      ; next word (literal)
      (= opcode 0x1f)
      [(next code) (first code)]
      :else ; literal value 0xffff-0x1e (-1 to 30)
      [code (dec (- opcode 0x20))])))
      
(defn disasm-basic-opcode [operation word code]
  "returns [rem-code result-str]"
  (let [[rest1 op2] (disasm-operand word code 2)
        [rest2 op1] (disasm-operand word rest1 1)]
    [rest2 (str operation " " op1 ", " op2)]))

(defn disasm-non-basic-opcode [word code]
  "returns [rem-code result-str]"
  (let [opcode (bit-and 0x1f (bit-shift-right word 5))]
    (if (non-basic-opcodes opcode)
      (let [[rest1 op1] (disasm-operand word code 2)]
        [rest1 (str (non-basic-opcodes opcode) " " (format-op op1))])
      [code (str "reserved opcode: " (format "0x%02x" opcode))])))

(defn disasm-data [word code]
  "returns [rem-code result-str]"
  [code (str "Unknown: " (hex word))])

(defn disasm-opcode [[word & code]]
  "returns [rem-code result-str]"  
  (let [opcode (bit-and 0x001F word)]
    (cond 
      ;non-basic-opcode
      (= 0 opcode) 
      (disasm-non-basic-opcode word code)
      ;basic opcode
      (opcodes opcode)
      (disasm-basic-opcode (opcodes opcode) word code)
      :else
      (disasm-data word code))))
     
(defn disasm [src-code offset]
  (loop [code src-code
         addr offset
         result []]
    (if code
      (let [[rem-code result-str] (disasm-opcode code)]
        (recur rem-code 
               (+ addr (- (count code) (count rem-code))) 
               (conj result [addr result-str])))
      result)))

(defn dump-asm [code-list & options]
  (cond (= :no-offsets (first options)) 
    (doseq [line code-list]
      (println (str (second line))))
    (= :dec-offsets (first options))
    (doseq [line code-list]
      (println (str (format "%04d" (first line)) ": " (second line))))
    :else
    (doseq [line code-list]
      (println (str (hex (first line)) ": " (second line))))))
  
            

