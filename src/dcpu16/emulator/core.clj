(ns dcpu16.emulator.core (:use [dcpu16.util] [dcpu16.asm.core]))

(load "common")

(def initial-regs {:A 0, :B 0, :C 0, :X 0, :Y 0, :Z 0, :I 0, :J 0
                   :PC 0, :SP 0, :EX 0})

(def cond-ops #{:IFB, :IFC, :IFE, :IFN, :IFG, :IFA, :IFL, :IFU})

(defn execute-1-instruction [mem regs])

(defn skip-1-instruction 
  "Skips one instruction and returns new state of [skipped-op mem regs]"
  [mem regs])

(defn add16 [a b]
  (let [tmpr (+ a b)]
    [(bit-shift-right tmpr 16) (bit-and tmpr 0xFFFF)]))

(defn sub16 [a b]
  (let [tmpr (- a b)
        tmpr (if (< tmpr 0) (long (+ (* 65536 65536) tmpr)) (long tmpr))] 
    [(bit-shift-right tmpr 16) (bit-and tmpr 0xFFFF)]))

(defn mul16 [a b]
   (let [tmpr (* a b)]
     (if (>= tmpr 65536) [(bit-and 0xFFFF (bit-shift-right tmpr 16)) (bit-and 0xFFFF tmpr)] [0 tmpr])))

(defn mli16 [a b]
   (let [[a sa] [(from-2s-complement a) (sgn a)]
         [b sb] [(from-2s-complement b) (sgn b)]
         tmpr (* a b)
         tmpr (if (not= sa sb) (- (* 65536 65536) tmpr) tmpr)]
     [(bit-and 0xFFFF (bit-shift-right tmpr 16)) (bit-and 0xFFFF tmpr)]))

(defn div16 [a b]
  (if (= 0 b)
    [0 0]
    (let [r (/ a b)]
      [(long (* 65536 (- r (long r)))) (long r)])))

(defn dvi16 [a b]
   (let [[a sa] [(from-2s-complement a) (sgn a)]
         [b sb] [(from-2s-complement b) (sgn b)]
         r (/ a b)
         restr (long (* 65536 (- r (long r))))
         [tmpr restr] (if (not= sa sb) 
                        [(long (- (* 65536 65536) r)) (- 65536 restr)] 
                        [(long r) restr])]
     [restr (bit-and tmpr 0xFFFF)]))

(defn mdi16 [a b]
  (to-2s-complement (rem (from-2s-complement a)
                         (from-2s-complement b)))) 

(defn shl16 [a b]
   (let [tmpr (bit-shift-left a b)]
     [(bit-and 0xFFFF (bit-shift-right (bit-shift-left a b) 16)) (bit-and 0xFFFF tmpr)]))

(defn shr16 [a b]
   (let [tmpr (bit-shift-right a b)]
     [(bit-and 0xFFFF (bit-shift-right (bit-shift-left a 16) b)) (bit-and 0xFFFF tmpr)]))

(defn asr16 [a b]
   (let [[a sa] [(from-2s-complement a) (sgn a)]
         tmpr (bit-shift-right a b)
         exr (bit-and 0xFFFF (bit-shift-right (bit-shift-left a 16) b))]
     [(to-2s-complement exr) (to-2s-complement tmpr)]))

(defn adx16 [a b ex]
  (let [tmpr (+ a b ex)]
    [(bit-shift-right tmpr 16) (bit-and tmpr 0xFFFF)]))

(defn sbx16 [a b ex]
  (let [tmpr (+ (- a b) ex)
        tmpr (if (< tmpr 0) (long (+ (* 65536 65536) tmpr)) (long tmpr))] 
    [(bit-shift-right tmpr 16) (bit-and tmpr 0xFFFF)]))

(defn emulator-error [msg]
  (throw (Exception. msg)))

(defn read-op [[op-type reg value] mem regs]
  "Reads a value from the location which is specified by the provided operand descriptor
   and returns [regs value]"
  (case op-type
    :REG [regs (regs reg)]
    :STACK
    (case reg
      :POP [(update-in regs [:SP] inc) (nth mem (regs :SP))]
      :PEEK [regs (nth mem (regs :SP))]
      :PUSH (emulator-error "Undefined behaviour for INSTRUCTION op1, PUSH"))
    :REGMEM [regs (nth mem (regs reg))]
    :REGLITMEM [regs (nth mem (bit-and 0xFFFF (+ value (regs reg))))]
    :LITMEM [regs (nth mem value)]
    :LIT [regs value]))

(defn store-op [[op-type reg value] mem regs target-value]
  "Stores the target-value to the location specified by the provided operand descriptor
   and returns the updated [mem regs]"
  (case op-type
    :REG [mem (assoc regs reg target-value)]
    :STACK
    (case reg
      :POP (emulator-error "Undefined behaviour for INSTRUCTION POP, operand2")
      :PEEK [(assoc-in mem [(regs :SP)] target-value) regs]
      :PUSH [(assoc-in mem [(dec (regs :SP))] target-value) (update-in regs [:SP] dec)])
    :REGMEM [(assoc-in mem [(regs reg)] target-value) regs]
    :REGLITMEM [(assoc-in mem [(+ value (regs reg))] target-value) regs]
    :LITMEM [(assoc-in mem [value] target-value) regs]
    :LIT [mem regs]))

(defmulti execute-basic-instruction* (fn [operation op1 op2 mem regs] (keyword operation)))

(defmethod execute-basic-instruction* :SET [operation op1 op2 mem regs]
  (let [[regs value] (read-op op2 mem regs)
        [mem regs] (store-op op1 mem regs value)]
    [mem regs]))

(defmethod execute-basic-instruction* :STI [operation op1 op2 mem regs]
  (let [[regs value] (read-op op2 mem regs)
        [mem regs] (store-op op1 mem regs value)
        newI (bit-and 0xFFFF (inc (regs :I)))
        newJ (bit-and 0xFFFF (inc (regs :J)))]
    [mem (assoc regs :I newI :J newJ)]))
     
(defmethod execute-basic-instruction* :STD [operation op1 op2 mem regs]
  (let [[regs value] (read-op op2 mem regs)
        [mem regs] (store-op op1 mem regs value)
        newI (to-2s-complement (dec (regs :I)))
        newJ (to-2s-complement (dec (regs :J)))]
    [mem (assoc regs :I (dec (regs :I)) :J (dec (regs :J)))]))

(defn math-op [op-func op1 op2 mem regs]
  (let [[regs value1] (read-op op1 mem regs)
        [regs value2] (read-op op2 mem regs)
        [carry result-value] (op-func value1 value2)
        regs (assoc regs :EX carry)]
    (store-op op1 mem regs result-value)))
 
(defmethod execute-basic-instruction* :ADD [operation op1 op2 mem regs]
  (math-op add16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :SUB [operation op1 op2 mem regs]
  (math-op sub16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :MUL [operation op1 op2 mem regs]
  (math-op mul16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :DIV [operation op1 op2 mem regs]
  (math-op div16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :SHL [operation op1 op2 mem regs]
  (math-op shl16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :SHR [operation op1 op2 mem regs]
  (math-op shr16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :ASR [operation op1 op2 mem regs]
  (math-op asr16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :DVI [operation op1 op2 mem regs]
  (math-op dvi16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :MLI [operation op1 op2 mem regs]
  (math-op mli16 op1 op2 mem regs))

(defn op-no-carry [op-func op1 op2 mem regs]
  (let [[regs value1] (read-op op1 mem regs)
        [regs value2] (read-op op2 mem regs)
        result-value (op-func value1 value2)
        [mem regs] (store-op op1 mem regs result-value)]
    [mem regs]))

(defmethod execute-basic-instruction* :MOD [operation op1 op2 mem regs]
  (op-no-carry mod op1 op2 mem regs))

(defmethod execute-basic-instruction* :MDI [operation op1 op2 mem regs]
  (op-no-carry mdi16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :AND [operation op1 op2 mem regs]
  (op-no-carry bit-and op1 op2 mem regs))

(defmethod execute-basic-instruction* :BOR [operation op1 op2 mem regs]
  (op-no-carry bit-or op1 op2 mem regs))

(defmethod execute-basic-instruction* :XOR [operation op1 op2 mem regs]
  (op-no-carry bit-xor op1 op2 mem regs))

(defn math-op-with-carry [op-func op1 op2 mem regs]
  (let [[regs value1] (read-op op1 mem regs)
        [regs value2] (read-op op2 mem regs)
        [carry result-value] (op-func value1 value2 (regs :EX))
        regs (assoc regs :EX carry)]
    (store-op op1 mem regs result-value)))

(defmethod execute-basic-instruction* :ADX [operation op1 op2 mem regs]
  (math-op-with-carry adx16 op1 op2 mem regs))

(defmethod execute-basic-instruction* :SBX [operation op1 op2 mem regs]
  (math-op-with-carry sbx16 op1 op2 mem regs))

(defn cond-op [cmp-fn op1 op2 mem regs]
 (let [[regs value1] (read-op op1 mem regs)
       [regs value2] (read-op op2 mem regs)]
   (if (cmp-fn value1 value2)
     [mem regs]
     (loop [skip-mem mem
            skip-regs regs
            cycles 0]
       (let [[op skip-mem skip-regs add-cycles] (skip-1-instruction skip-mem skip-regs)]
         (if (cond-ops op)
           (recur skip-mem skip-regs (+ cycles add-cycles)) ; continue skipping, if the last skipped operation was an IFx
           [mem (assoc regs :PC (skip-regs :PC)) (+ cycles add-cycles)]))))))

(defmethod execute-basic-instruction* :IFE [operation op1 op2 mem regs]
  (cond-op #(= %1 %2) op1 op2 mem regs))

(defmethod execute-basic-instruction* :IFN [operation op1 op2 mem regs]
  (cond-op #(not= %1 %2) op1 op2 mem regs))

(defmethod execute-basic-instruction* :IFG [operation op1 op2 mem regs]
  (cond-op #(> %1 %2) op1 op2 mem regs))

(defmethod execute-basic-instruction* :IFL [operation op1 op2 mem regs]
  (cond-op #(< %1 %2) op1 op2 mem regs))

(defmethod execute-basic-instruction* :IFA [operation op1 op2 mem regs]
  (cond-op #(> (from-2s-complement %1) (from-2s-complement %2)) op1 op2 mem regs))

(defmethod execute-basic-instruction* :IFU [operation op1 op2 mem regs]
  (cond-op #(< (from-2s-complement %1) (from-2s-complement %2)) op1 op2 mem regs))

(defmethod execute-basic-instruction* :IFB [operation op1 op2 mem regs]
  (cond-op #(not= 0 (bit-and %1 %2)) op1 op2 mem regs))

(defmethod execute-basic-instruction* :IFC [operation op1 op2 mem regs]
  (cond-op #(zero? (bit-and %1 %2)) op1 op2 mem regs))

(defmulti execute-non-basic-instruction* (fn [operation op mem regs] (keyword operation)))

(defmethod execute-non-basic-instruction* :JSR [operation op mem regs]
  (let [[regs value] (read-op op mem regs)
        [mem regs] (store-op [:STACK :PUSH nil] mem regs (regs :PC))]
    [mem (assoc regs :PC value)]))

(defmethod execute-non-basic-instruction* :default [operation op mem regs]
  [mem regs])

(defn read-operand [opcode word mem regs op-num]
  "Reads an operand and returns [[operand-type reg value cycles] regs]
   operand-type can be :STACK :REG, :REGMEM, :LITMEM, :REGLITMEM and :LIT
   reg will contain the (stack) register keyword (:A, :B, :C, etc.)
   value will contain either the literal value or the memory address"
  (cond
      ; register
      (or (in-range? opcode 0x00 0x07)
          (in-range? opcode 0x1b 0x1d))
      [[:REG (keyword (regopcodes opcode)) nil (op-access-cycles :REG)] regs]
      ; stack (register) -> PUSH
      (and (= 1 op-num) (= 0x18 opcode))
      [[:STACK :PUSH nil (op-access-cycles :PUSHPOP)] regs]
      ; stack (register) -> POP
      (and (= 2 op-num) (= 0x18 opcode))
      [[:STACK :POP nil (op-access-cycles :PUSHPOP)] regs]
       ; stack (register) -> PEEK
      (= 0x19 opcode)
      [[:STACK :PEEK nil (op-access-cycles :PEEK)] regs]     
      ; [register]
      (in-range? opcode 0x08 0x0f)
      [[:REGMEM (keyword (regopcodes (- opcode 0x08))) nil (op-access-cycles :REGMEM)] regs] 
      ; [SP + next word]
      (= opcode 0x1a)
      [[:REGLITMEM :SP (nth mem (regs :PC)) (op-access-cycles :REGLITMEM)] (update-in regs [:PC] inc)]
      ; [next word + register]
      (in-range? opcode 0x10 0x17)
      [[:REGLITMEM (keyword (regopcodes (- opcode 0x10))) (nth mem (regs :PC)) (op-access-cycles :REGLITMEM)] (update-in regs [:PC] inc)]
      ; [next word]
      (= opcode 0x1e)
      [[:LITMEM nil (nth mem (regs :PC)) (op-access-cycles :LITMEM)] (update-in regs [:PC] inc)]
      ; next word (literal)
      (= opcode 0x1f)
      [[:LIT nil (nth mem (regs :PC)) (op-access-cycles :LIT)] (update-in regs [:PC] inc)]
      :else ; literal value 0x00-0x1f
      (let [val (dec (- opcode 0x20))]
        [[:LIT nil (if (< val 0) (+ val 65536) val) (op-access-cycles :SHORTLIT)] regs])))


(defn skip-basic-instruction 
  "Skips a basic instruction and returns [skipped-operation mem regs]"
  [operation word mem regs]
  (let [[op2 regs] (read-operand (bit-shift-right word 10) word mem regs 2)
        [op1 regs] (read-operand (bit-and 0x1f (bit-shift-right word 5)) word mem regs 1)]
    [(keyword operation) mem regs (special-cycles :IFSKIP)]))

(defn skip-non-basic-instruction 
  "Skips a non-basic instruction and returns [0 mem regs]"
  [word mem regs]
  (let [opcode (bit-and 0x1f (bit-shift-right word 5))
        operation (non-basic-opcodes opcode)
        [op regs] (read-operand (bit-shift-right word 10) word mem regs 2)]
    [0 mem regs (special-cycles :IFSKIP)]))

(defn skip-1-instruction 
  "Skips one instruction and returns new state of [skipped-op mem regs cycles]"
  [mem regs]
  (let [word (nth mem (regs :PC))
        opcode (bit-and 0x001F word)
        operation (opcodes opcode)]
    (if operation
      (skip-basic-instruction operation word mem (update-in regs [:PC] inc))
      (skip-non-basic-instruction word mem (update-in regs [:PC] inc)))))

(defn execute-basic-instruction [operation word mem regs]
  "Executes a basic instruction and returns new state of [mem regs]"
  (let [[op2 regs] (read-operand (bit-shift-right word 10) word mem regs 2)
        [op1 regs] (read-operand (bit-and 0x1f (bit-shift-right word 5)) word mem regs 1)
        [mem regs add-cycles] (execute-basic-instruction* operation op1 op2 mem regs)
        op1-cycles (last op1)
        op2-cycles (last op2)
        operation-cycles (op-cycles (keyword operation))]
    [mem regs (+ operation-cycles op1-cycles op2-cycles (if add-cycles add-cycles 0))]))

(defn execute-non-basic-instruction [word mem regs]
  "Executes a non-basic instruction and returns new state of [mem regs]"
  (let [opcode (bit-and 0x1f (bit-shift-right word 5))
        operation (non-basic-opcodes opcode)
        [op regs] (read-operand (bit-shift-right word 10) word mem regs 2)
        [mem regs add-cycles] (execute-non-basic-instruction* operation op mem regs)
        operand-cycles (last op)
        operation-cycles (op-cycles (keyword operation))]
    (if operation
      [mem regs (+ operation-cycles operand-cycles (if add-cycles add-cycles 0))]
      [mem regs 1])))

(defn execute-1-instruction [mem regs]
  "Executes one instruction and returns new state of [mem regs cycles]"
  (let [word (nth mem (regs :PC))
        opcode (bit-and 0x001F word)
        operation (opcodes opcode)]
    (if operation
      (execute-basic-instruction operation word mem (update-in regs [:PC] inc))
      (execute-non-basic-instruction word mem (update-in regs [:PC] inc)))))

(defn run
  "Starts the emulator
   Options: :trace"
   [mem emu-options]
  (let [start-time (. System (nanoTime))]
    (println "* My Emulator *")
    (when (emu-options :trace) 
      (println "Code                     Cycles     Modified registers"))
    (loop [mem mem
           regs initial-regs
           total-cycles 0]
      (let [[new-mem new-regs cycles] (execute-1-instruction mem regs)]
        (when (emu-options :trace) 
          (println (format "%-24s %-10s %s"  
                           (second (first (disasm (subvec mem (regs :PC) (+ 3 (regs :PC))) 0)))
                           cycles
                           (reg-diff regs new-regs)))
          (when (not= mem new-mem)
            (dump-mem (mem-diff mem new-mem))))
        
        (if (or (>= (/ (double (- (. System (nanoTime)) start-time)) 1000000.0) 5000.0) ; check timeout
                (> (regs :PC) 60) 
                (= (regs :PC) (new-regs :PC))) ; detect endless loop and stop the emulation
          [new-mem regs total-cycles]
          (recur new-mem new-regs (+ total-cycles cycles)))))))

(load "org")
