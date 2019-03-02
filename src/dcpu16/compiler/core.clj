(ns dcpu16.compiler.core (:use [dcpu16.asm.core] [dcpu16.util] [dcpu16.emulator.core]))

(defn compiler-error [msg]
  (throw (Exception. msg)))

(load "regs")
(load "stack_helper")
(load "reg_usage")

(defmulti parse-list 
  "Returns [next-source main-code func-code data-code symbols result-info]"
  (fn [expr source symbols reg-info stack-info pref-result] (first expr)))

(def store-types #{:tmp-reg :result-reg :constant :mem})


(def data-types {'int :int,  'word :word
                 'dword :dword, 'long :long
                 'string :string,  'list :list, 'vector :vector
                 'func :func,  'fix :fix})

(def data-type-sizes {:int 1, :long 2, :word 1, :dword 2
                      :string 1, :list 1, :vector 1
                      :func 1, :fix 2})

(def commutative-operations #{:ADD :BOR :XOR :AND :MUL})

(defn commutative-operation? [op]
  (boolean (commutative-operations op)))


(defn get-data-type [op]
  (cond
    ; number
    (number? op)
    (cond (float? op) :fix
          (and (< op 0) (>= op -32768)) :int
          (< op 0) :long
          (> op 65535) :dword
          :else :word)
    :else 
    (syntax-error (str "Unexpected operand: " op))))
    
(defn get-data-type-size [data-type]
  {:post [%]}
  (data-type-sizes data-type))

(defn parse-numeric-operand [op symbols reg-info stack-info]
  (let [data-type (get-data-type op)]
    [nil nil nil {:loc op, :type :constant, :data-type data-type, :size (get-data-type-size data-type)} reg-info]))

(defn parse-expr-operand [op symbols reg-info stack-info pref-result]
  (let [[_ mcode fcode dcode _ result-info reg-info] (parse-list op nil symbols reg-info stack-info pref-result)]
    [mcode fcode dcode result-info reg-info]))

(defn get-var-data-type [type-symbol]
  (let [data-type (data-types type-symbol)]
    (assert data-type (str "Unknown data type: " type-symbol))
    data-type))

(defn get-data-type-size [data-type]
  (let [size (data-type-sizes data-type)]
    (assert size (str "Unknown data type: " data-type))
    size))

(defn numeric-type? [data-type]
  (boolean
    (#{:word :int :dword :long :fix} data-type)))

(defn integer-type? [data-type]
  (boolean
    (#{:word :int :dword :long} data-type)))

(defmulti convert-constant (fn [result-info target-data-type]
                             (if (= target-data-type (result-info :data-type))
                               :no-conversion-necessary
                               target-data-type)))
                                    
(defmethod convert-constant :no-conversion-necessary [result-info target-data-type]
  result-info)

(defmethod convert-constant :word [result-info target-data-type]
  (let [source-data-type (result-info :data-type)
        source-value (result-info :loc)]
    (assert (integer-type? source-data-type) (str "Constant of data type " source-data-type " cannot be converted to word."))
    (assert (in-range? source-value 0 65535) (str "Constant value " source-value " is out of range for target data type word [0 - 65535]."))
    (assoc result-info :data-type target-data-type :size (get-data-type-size target-data-type)))) 

(defmethod convert-constant :int [result-info target-data-type]
  (let [source-data-type (result-info :data-type)
        source-value (result-info :loc)]
    (assert (integer-type? source-data-type) (str "Constant of data type " source-data-type " cannot be converted to int."))
    (assert (in-range? source-value -32768 32767) (str "Constant value " source-value " is out of range for target data type int [-32768 - 32767]."))
    (assoc result-info :data-type target-data-type :size (get-data-type-size target-data-type))))

(defmethod convert-constant :long [result-info target-data-type]
  (let [source-data-type (result-info :data-type)
        source-value (result-info :loc)]
    (assert (integer-type? source-data-type) (str "Constant of data type " source-data-type " cannot be converted to long."))
    (assert (in-range? source-value -2147483648 2147483647) (str "Constant value " source-value " is out of range for target data type long [-2147483648 - 2147483647]."))
    (assoc result-info :data-type target-data-type :size (get-data-type-size target-data-type))))
 
(defmethod convert-constant :dword [result-info target-data-type]
  (let [source-data-type (result-info :data-type)
        source-value (result-info :loc)]
    (assert (integer-type? source-data-type) (str "Constant of data type " source-data-type " cannot be converted to dword."))
    (assert (in-range? source-value 0 4294967295) (str "Constant value " source-value " is out of range for target data type int [0 4294967295]."))
    (assoc result-info :data-type target-data-type :size (get-data-type-size target-data-type))))

(defmethod convert-constant :fix [result-info target-data-type]
  (let [source-data-type (result-info :data-type)
        source-value (result-info :loc)]
    (assert (numeric-type? source-data-type) (str "Constant of data type " source-data-type " cannot be converted to fix."))
    (assert (in-range? source-value -32768 32767) (str "Constant value " source-value " is out of range for target data type fix [-32767 - 32768]."))
    (assoc result-info :data-type target-data-type :loc (float (result-info :loc)) :size (get-data-type-size target-data-type))))
 
(defmethod convert-constant :default [result-info target-data-type]
 (compiler-error (str "Cannot convert from data type " (result-info :data-type) " to " target-data-type))) 
  
(defmulti convert-register (fn [result-info target-data-type]
                             (if (= target-data-type (result-info :data-type))
                               :no-conversion-necessary
                               [(result-info :data-type) target-data-type])))         

(defmethod convert-register :no-conversion-necessary [result-info target-data-type]
  result-info)

(defmethod convert-register :default [result-info target-data-type]
 (compiler-error (str "Cannot convert from data type " (result-info :data-type) " to " target-data-type))) 

(defmulti convert-mem (fn [result-info target-data-type]
                        (if (= target-data-type (result-info :data-type))
                          :no-conversion-necessary
                          [(result-info :data-type) target-data-type])))         

(defmethod convert-mem :no-conversion-necessary [result-info target-data-type]
  result-info)

(defmethod convert-mem :default [result-info target-data-type]
 (compiler-error (str "Cannot convert from data type " (result-info :data-type) " to " target-data-type))) 

(defn convert-result-info [result-info target-data-type]
  "Converts the result described by the given result-info to the required data type if necessary and possible"
  (cond
    (= :constant (result-info :type))
    (convert-constant result-info target-data-type)
    (#{:tmp-reg :result-reg} (result-info :type))
    (convert-register result-info target-data-type)
    (= :mem (result-info :type))
    (convert-mem result-info target-data-type)
    :else
    (compiler-error (str "(convert-result-info) Not yet implemented: " (result-info :type)))))


(defn get-stack-var-addr [v stack-info]
  (let [offset (reduce + (take (v :stack-depth) (stack-info :offsets)))]
    [:SP (to-2s-complement (- (v :stack-pos) offset))]))

(defn get-stack-par-addr [v stack-info]
  [:SP (to-2s-complement (dec (v :stack-pos)))])

(defn get-constant-word-output [result-info]
  "Returns the constant value separated into one or two words (16 bit)
   [word1 word2]"
  (let [v (result-info :loc)]
    (cond 
      ; 16 bit values
      (= 1 (result-info :size))
      (case (result-info :data-type)
        :word [(result-info :loc)]
        :int (if (< v 0) [(+ v 65536)] [v]))
      ; 32 bit values
      (= 2 (result-info :size))
      (case (result-info :data-type)
        :long (if-bind (< v 0) [nv (+ v 4294967296) v]
                       [(bit-and nv 0x0000FFFF) (bit-shift-right nv 16)])
        :dword [(bit-and v 0x0000FFFF) (bit-shift-right v 16)]
        :fix (if-bind (< v 0) [nv (+ v 65536) v]
                       [(int nv) (Math/abs (int (* 65536 (- (int v) v))))]))
      :else
      (compiler-error (str "(get-constant-word-output) Unexpected size for result-info :" (result-info :size))))))
  
(defn get-result-info-word-output [result-info]
  "Returns the result value separated into one or two words (16 bit)
   [word1 word2]"
  (if (= :constant (result-info :type))
    (get-constant-word-output result-info)
    (cond 
      (= 1 (result-info :size))
      [(result-info :loc)]
      (= 2 (result-info :size))
      (case (result-info :type)
        :tmp-reg [(result-info :loc) (result-info :loc2)]
        :result-reg [(result-info :loc) (result-info :loc2)]
        :mem [(result-info :loc) (result-info :loc2)])
      :else
      (compiler-error (str "(get-result-info-word-output) Unexpected size for result-info :" (result-info :size))))))
 

(defn get-var-result-info [symbols var-name reg-info stack-info]
  ; returns [mcode fcode dcode result-info reg-info] for getting the variable value
  {:pre [(symbols var-name)]}
  (let [v (symbols var-name)]
    (case (v :type)
      :stack-var
      (let [stack-addr (get-stack-var-addr v stack-info)
            stack-addr2 (if (= 2 (v :size)) (conj stack-addr 1))]
        [nil nil nil {:loc stack-addr, :loc2 stack-addr2, :type :mem, :size (v :size) :data-type (v :data-type)} reg-info])
      :parameter
      (let [stack-addr (get-stack-par-addr v stack-info)
            stack-addr2 (if (= 2 (v :size)) (conj stack-addr 1))]
        [nil nil nil {:loc stack-addr, :loc2 stack-addr2, :type :mem, :size (v :size) :data-type (v :data-type)} reg-info])
      :constant
      [nil nil nil {:loc (v :value), :type :constant, :size (v :size) :data-type (v :data-type)} reg-info])))

(defn parse-symbol-operand [op symbols reg-info stack-info]
  (let [symb (symbols op)]
    (assert symb (str "Symbol is not defined: " op))
    (get-var-result-info symbols op reg-info stack-info)))

(defn parse-operand 
  "Returns [mcode fcode dcode result-info reg-info]
   result-info is a map which contains the location (:loc, for 2 words size: :loc and :loc2) where the result will be stored
   , the type (:type) of the result (e.g. constant, register), data type (:data-type) and the size of the result in words [:size], e.g.
   int, string, list, vector, etc."
  [op symbols reg-info stack-info pref-result]
  (let [[mcode fcode dcode result-info reg-info2] (cond 
    (number? op) (parse-numeric-operand op symbols reg-info stack-info)
    (list? op) (parse-expr-operand op symbols reg-info stack-info pref-result)
    (symbol? op) (parse-symbol-operand op symbols reg-info stack-info)
    :else (syntax-error (str "(parse-operand) Unknown operand: " op)))]
    [mcode fcode dcode result-info reg-info2]))



(defn gen-load-operator 
  "Generates code for loading the given result value.
   Returns [load-op-code out-result-info reg-info]"
  [in-result-info reg-info pref-result]
  (cond 
    ; Register
    (= :tmp-reg (in-result-info :type))
    [nil in-result-info reg-info]
    (= :result-reg (in-result-info :type))
    (if (= :tmp-reg pref-result)
      (if (= 2 (in-result-info :size))
        (let [[reg-info reg1 reg2] (reserve-2-regs reg-info)
              out-result-info (assoc in-result-info :type :tmp-reg :loc reg1 :loc2 reg2)]
          [(concat (asm :SET reg1, :A)
                   (asm :SET reg2, :B)) out-result-info reg-info])
        (let [[reg-info reg] (reserve-reg reg-info)
              out-result-info (assoc in-result-info :type :tmp-reg :loc reg)]
          [(asm :SET reg, :A) out-result-info reg-info]))
      [nil in-result-info reg-info])
    ; Constant or memory location
    (#{:constant :mem} (in-result-info :type))
    (case (in-result-info :size)
      1 (let [[reg-info result-reg] (if (= :result-reg pref-result) [reg-info :A] (reserve-reg reg-info))
              [word1 word2] (get-result-info-word-output in-result-info)]
          [(asm :SET result-reg, word1) 
           {:type pref-result, :data-type (in-result-info :data-type), :loc result-reg 
            :size 1} reg-info])
      2 (let [[reg-info result-reg1 result-reg2] (if (= :result-reg pref-result) 
                                                   [reg-info :A :B] 
                                                   (reserve-2-regs reg-info))
              [word1 word2] (get-result-info-word-output in-result-info)]
          [(concat (asm :SET result-reg1, word1)
                   (asm :SET result-reg2, word2))
           {:type pref-result :data-type (in-result-info :data-type) :loc result-reg1 :loc2 result-reg2 
            :size 2}
           reg-info]))
    :else
    (compiler-error (str "(gen-load-operator) Unexpected type :" (in-result-info :type)))))

(defmulti gen-math-operation 
  "Generates code for applying the given operation to the operands
   Returns: [math-code expr-result-info]"
  (fn [asm-op op1-info op2-info] (op1-info :data-type)))

(defmethod gen-math-operation :int [asm-op op1-info op2-info]
  (let [[op2-word1 _] (get-result-info-word-output op2-info)]
    (cond 
      (= :MUL asm-op)
      [(asm :MLI (op1-info :loc), op2-word1) op1-info]
      (= :DIV asm-op)
      [(asm :DVI (op1-info :loc), op2-word1) op1-info]
      (= :SHR asm-op)
      [(asm :ASR (op1-info :loc), op2-word1) op1-info]
      :else
      [(asm asm-op (op1-info :loc), op2-word1) op1-info])))

(defmethod gen-math-operation :word [asm-op op1-info op2-info]
  (let [[op2-word1 _] (get-result-info-word-output op2-info)]
    [(asm asm-op (op1-info :loc), op2-word1) op1-info]))

(defmethod gen-math-operation :dword [asm-op op1-info op2-info]
  (let [[op2-word1 op2-word2] (get-result-info-word-output op2-info)
        op1-word1 (op1-info :loc)
        op1-word2 (op1-info :loc2)]
    [(cond 
       ; Operations that require special handling or can be optimized in certain situations:
       (#{:SUB :ADD} asm-op)
       (cond
         ; Optimization? 16 bit constant operand2
         (and (= :constant (op2-info :type)) (zero? op2-word2))
         (concat (asm asm-op op1-word1, op2-word1)
                 (asm :ADD op1-word2 :EX))
         :else
         (concat (asm asm-op op1-word1, op2-word1)
                 (asm ({:SUB :SBX, :ADD :ADX} asm-op) op1-word2, op2-word2)))
       (= :MUL asm-op) 
       (cond
         ; Optimization? 16-bit constant operand2
         (and (= :constant (op2-info :type)) (zero? op2-word2))
         (concat (asm :MUL op1-word2, op2-word1)
                 (asm :MUL op1-word1, op2-word1)
                 (asm :ADD op1-word2, :EX))
         
         ; Optimization? 17-bit constant < 01FFFF 
         (and (= :constant (op2-info :type)) (= 1 op2-word2) (not= 0xFFFF op1-word1))
         (concat (asm :MUL op1-word2, (inc op2-word1))
                 (asm :ADD op1-word2, op1-word1)
                 (asm :MUL op1-word1, op2-word1)
                 (asm :ADD op1-word2, :EX))         
         
         ; Optimization? 32-bit constant and word1 + word2 < 65536
         (and (= :constant (op2-info :type)) (< (+ op2-word1 op2-word2) 65536))
         (concat (asm :MUL op1-word2, (+ op2-word1 op2-word2))
                 (asm :SET :PUSH, op1-word1)
                 (asm :MUL :PEEK, op2-word2)
                 (asm :ADD op1-word2, :POP)
                 (asm :MUL op1-word1, op2-word1)
                 (asm :ADD op1-word2, :EX))         
          
         :else
         (concat (asm :SET :PUSH, op1-word2)
                 (asm :MUL :PEEK, op2-word2)
                 (asm :MUL op1-word2, op2-word1)
                 (asm :ADD op1-word2, :POP)
                 (asm :SET :PUSH, op1-word1)
                 (asm :MUL :PEEK, op2-word2)
                 (asm :ADD op1-word2, :POP)
                 (asm :MUL op1-word1, op2-word1)
                 (asm :ADD op1-word2, :EX)))
       ; Operations that do not require special handling:
       :else
       (concat (asm asm-op op1-word1, op2-word1)
               (asm asm-op op1-word2, op2-word2))) op1-info]))


(defmethod gen-math-operation :default [asm-op op1-info op2-info]
  (compiler-error (str "(gen-math-operation) Not yet implemented for data type " (op1-info :data-type))))

(defn prepare-operands [op1 mcode1 fcode1 dcode1 result-info1
                        op2 mcode2 fcode2 dcode2 result-info2 reg-info
                        symbols stack-info pref-result is-commutative-operation]
  "Returns [mcode1 fcode1 dcode1 result-info1
            mcode2 fcode2 dcode2 result-info2 nnreg-info]"
  (let [type1 (result-info1 :type)
        type2 (result-info2 :type)]
    (case [type1 type2]
      [:constant :constant]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:constant :tmp-reg]
      (if is-commutative-operation
        [mcode2 fcode2 dcode2 result-info2 mcode1 fcode1 dcode1 result-info1 reg-info]
        [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info])
      [:constant :result-reg]
      (if is-commutative-operation
        [mcode2 fcode2 dcode2 result-info2 mcode1 fcode1 dcode1 result-info1 reg-info]
        [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info])
      [:constant :mem]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:tmp-reg :constant]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:tmp-reg :tmp-reg]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:tmp-reg :result-reg]
      (if is-commutative-operation
        [mcode2 fcode2 dcode2 result-info2 mcode1 fcode1 dcode1 result-info1 reg-info]
        [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info])
      [:tmp-reg :mem]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:result-reg :constant]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:result-reg :tmp-reg]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:result-reg :result-reg]
      (let [[mcode2 fcode2 dcode2 result-info2 reg-info] (parse-operand op2 symbols reg-info stack-info :tmp-reg)
            [mcode1 fcode1 dcode1 result-info1 reg-info] (if (= :result-reg (result-info2 :type))
                                                           (parse-operand op1 symbols reg-info stack-info :tmp-reg)
                                                           [mcode1 fcode1 dcode1 result-info1 reg-info])]
        [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info])
      [:result-reg :mem]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:mem :constant]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info]
      [:mem :tmp-reg]
      (if is-commutative-operation
        [mcode2 fcode2 dcode2 result-info2 mcode1 fcode1 dcode1 result-info1 reg-info]
        [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info])
      [:mem :result-reg]
      (if is-commutative-operation
        [mcode2 fcode2 dcode2 result-info2 mcode1 fcode1 dcode1 result-info1 reg-info]
        [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info])
      [:mem :mem]
      [mcode1 fcode1 dcode1 result-info1 mcode2 fcode2 dcode2 result-info2 reg-info])))
      
(defn handle-math-operators [asm-operation ifunc expr source symbols reg-info stack-info pref-result]
  (let [op1 (second expr)
        op2 (second (next expr))
        rem-expr (nthnext expr 3)]
    (assert (nil? rem-expr) "(+ op1 op2) only supports two operands!")
    (let [[mcode1 fcode1 dcode1 result-info1 reg-info] (parse-operand op1 symbols reg-info stack-info pref-result)
          [mcode2 fcode2 dcode2 result-info2 reg-info] (parse-operand op2 symbols reg-info stack-info pref-result)
          [mcode1 fcode1 dcode1 result-info1
           mcode2 fcode2 dcode2 result-info2 reg-info] (prepare-operands op1 mcode1 fcode1 dcode1 result-info1
                                                                         op2 mcode2 fcode2 dcode2 result-info2 reg-info
                                                                         symbols stack-info pref-result (commutative-operation? asm-operation))]
           
      
      
      ;(coerce-operand-types ... TBD
      ;(optimize-operands ... TBD
      (if (= :constant (result-info1 :type) (result-info2 :type))
        [(next source) nil nil nil symbols
         {:loc (ifunc (result-info1 :loc) (result-info2 :loc)), :type :constant, :data-type (result-info1 :data-type) :size (result-info1 :size)} reg-info]
      (let [[load-op1-code op1-info reg-info] (gen-load-operator result-info1 reg-info pref-result)
            op2-info (convert-result-info result-info2 (op1-info :data-type))
            [math-code expr-result-info] (gen-math-operation asm-operation op1-info op2-info)
            ;pre-reg-code1  (when (= :tmp-reg (op1-info :type)) (pre-reg-usage reg-info (op1-info :loc) (op1-info :loc2)))
            pre-reg-code2  (when (= :tmp-reg (op2-info :type)) (pre-reg-usage reg-info (op2-info :loc) (op2-info :loc2)))
            post-reg-code2 (when (= :tmp-reg (op2-info :type)) (post-reg-usage reg-info (op2-info :loc) (op2-info :loc2)))
            post-reg-info (if (= :tmp-reg (op2-info :type)) 
                            (unreserve-regs reg-info (op2-info :loc) (op2-info :loc2))
                            reg-info)]
        [source (concat mcode1 pre-reg-code2 mcode2 load-op1-code math-code post-reg-code2) 
         (concat fcode1 fcode2)
         (concat dcode1 dcode2)
         symbols
         expr-result-info post-reg-info stack-info])))))                         

(defmethod parse-list '+ [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :ADD + expr source symbols reg-info stack-info pref-result))

(defmethod parse-list '* [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :MUL * expr source symbols reg-info stack-info pref-result))

(defmethod parse-list '/ [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :DIV / expr source symbols reg-info stack-info pref-result))

(defmethod parse-list '- [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :SUB - expr source symbols reg-info stack-info pref-result))

(defmethod parse-list 'mod [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :MOD mod expr source symbols reg-info stack-info pref-result))

(defmethod parse-list 'shl [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :SHL bit-shift-left expr source symbols reg-info stack-info pref-result))

(defmethod parse-list 'shr [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :SHR bit-shift-right expr source symbols reg-info stack-info pref-result))

(defmethod parse-list 'bit-and [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :AND bit-and expr source symbols reg-info stack-info pref-result))

(defmethod parse-list 'bit-or [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :BOR bit-or expr source symbols reg-info stack-info pref-result))

(defmethod parse-list 'bit-xor [expr source symbols reg-info stack-info pref-result]
  (handle-math-operators :XOR bit-xor expr source symbols reg-info stack-info pref-result))


(defn extract-binding [bind symbols reg-info stack-amount stack-info pref-result]
 "Returns [var-name mcode fcode dcode result-info stack-pos new-stack-amount]"
 (let [[mcode fcode dcode result-info _ _] (parse-operand (third bind) symbols reg-info stack-info pref-result)
       is-mutable (= \! (last (str (second bind))))]
   (cond 
     (#{:tmp-reg :result-reg :mem} (result-info :type))
     [(second bind) (first bind) mcode fcode dcode result-info stack-amount (+ stack-amount (result-info :size))]
     (= :constant (result-info :type))
     [(second bind) (first bind) mcode fcode dcode result-info stack-amount (if is-mutable 
                                                                              (+ stack-amount (result-info :size))
                                                                              stack-amount)])))


(defn register-var [symbols scope var-data-type var-type var-name result-info stack-pos stack-info]
  (let [data-type (get-var-data-type var-data-type)
        size (get-data-type-size data-type)
        is-mutable (= \! (last (str var-name)))
        var-name (if is-mutable (symbol (subs (str var-name) 0 (dec (count (str var-name))))) var-name)]
    (case var-type
      :constant
      (let [cvalue (convert-constant result-info data-type)]
        (assoc symbols var-name {:scope scope
                                 :type var-type
                                 :data-type data-type
                                 :value (cvalue :loc)
                                 :size size
                                 :is-mutable false}))
      :stack-var
      (assoc symbols var-name {:scope scope
                               :type var-type
                               :data-type data-type
                               :stack-pos stack-pos
                               :size size
                               :stack-depth (stack-info :depth)
                               :is-mutable is-mutable})
      :else
      (compiler-error (str "(register-var) Unexpected var-type: " var-type)))))



(defn generate-var-set [symbols var-name result-info stack-info reg-info]
  ; returns ASM opcodes for setting the variable to the value specified by result-info
  {:pre [(symbols var-name)]}
  (let [v (symbols var-name)
        c-result-info (convert-result-info result-info (v :data-type))
        [word1 word2] (get-result-info-word-output c-result-info)]
    (case (v :type)
      :stack-var
      (let [stack-addr (get-stack-var-addr v stack-info)]
        (case (v :size)
          1
          (asm :SET stack-addr, word1)
          2
          (concat (asm :SET stack-addr, word1)
                  (asm :SET (conj stack-addr 1), word2))))
      :constant
      (compiler-error (str "(generate-var-set) Unsupported var-type for set: " (v :type))))))
    
(defn generate-local-binding [var-name var-data-type mcode fcode dcode result-info stack-pos symbols stack-info reg-info]
  "Returns [symbols mlocal-var-code fcode dcode]"
  (let [is-mutable (= \! (last (str var-name)))
        usage-var-name (if is-mutable (symbol (subs (str var-name) 0 (dec (count (str var-name))))) var-name)]
    (cond 
      ; Register, memory location or mutable variable => Var needs to be stored on the stack
      (or (#{:result-reg :tmp-reg :mem} (result-info :type)) is-mutable)
      (let [symbols (register-var symbols :local var-data-type :stack-var var-name result-info stack-pos stack-info)]
        [symbols (concat mcode (generate-var-set symbols usage-var-name result-info stack-info reg-info)) fcode dcode])
      ; Constants: Var does not need to be stored on the stack
      (= :constant (result-info :type))
      [(register-var symbols :local var-data-type :constant var-name result-info stack-pos stack-info) mcode fcode dcode]
      :else
      (compiler-error (str "Unexpected internal result-type: " (result-info :type))))))
  
(defn parse-expressions [expressions symbols reg-info stack-info pref-result]
  "Returns [mcode fcode dcode result-info stack-info] -> result-info for the last expression"
  (loop [expressions expressions
         mcode []
         fcode []
         dcode []
         result-info nil]
    (if expressions
      (let [[mexpr-code fexpr-code dexpr-code result-info reg-info] (parse-operand (first expressions) symbols reg-info stack-info pref-result)]
        (recur (next expressions)
               (concat mcode mexpr-code)
               (concat fcode fexpr-code)
               (concat dcode dexpr-code)
               result-info))
      [mcode fcode dcode result-info])))

(defn parse-bindings [bindings symbols reg-info stack-info]
  "Returns [symbols local-var-mcode local-var-fcode local-var-dcode]"
   (loop [bindings bindings
          local-var-mcode []
          local-var-fcode []
          local-var-dcode []
          stack-amount 0
          symbols symbols]
     (if bindings
       (let [[var-name var-data-type mcode fcode dcode result-info stack-pos stack-amount] (extract-binding (take 3 bindings) symbols reg-info stack-amount stack-info :result-reg)
             [symbols var-mcode var-fcode var-dcode] (generate-local-binding var-name var-data-type mcode fcode dcode result-info stack-pos symbols stack-info reg-info)]
         (recur (nnnext bindings)
                (concat local-var-mcode var-mcode)
                (concat local-var-fcode var-fcode)
                (concat local-var-dcode var-dcode)
                stack-amount
                symbols))
       [symbols local-var-mcode local-var-fcode local-var-dcode])))

(defn update-stack-info [stack-info required-stack-size]
  (if (zero? required-stack-size)
    stack-info
    (update-in (update-in stack-info [:depth] inc)
               [:offsets] #(conj % required-stack-size))))

(defmethod parse-list 'let [expr source symbols reg-info stack-info pref-result]
  "Syntax for let:
   (let [bindings*] expressions*)
   Variables whose names end with a ! will be marked as mutable"
  (do
    (assert (vector? (second expr)) (str "let expects a vector for its bindings: " expr))
    (assert (third expr) (str "let: expression is missing"))
    (let [var-data-types (map get-var-data-type (take-nth 3 (second expr)))
          required-stack-size (reduce #(+ % (get-data-type-size %2)) 0 var-data-types)
          stack-info (update-stack-info stack-info required-stack-size)
          [symbols local-var-mcode local-var-fcode local-var-dcode] (parse-bindings (second expr) symbols reg-info stack-info)]
      (if-bind (> required-stack-size 0)
        [init-code (concat (asm :SUB :SP, required-stack-size))
                   []
         exit-code (concat (asm :ADD :SP, required-stack-size)) []]
        (let [[expr-mcode expr-fcode expr-dcode result-info] (parse-expressions (nnext expr) symbols reg-info stack-info pref-result)]
          [source 
           (concat init-code local-var-mcode expr-mcode exit-code) 
           (concat local-var-fcode expr-fcode)
           (concat local-var-dcode expr-dcode)
           symbols result-info])))))

(defn extract-parameter-definitions [par-vec]
  ; returns [{:name par-name, :data-type par-data-type, :pos par-position}*]
  (do
    (assert (even? (count par-vec)) (str "Incomplete parameter definition (expects data-type + parameter name): " par-vec))
    (map-indexed (fn [idx par-tuple] {:name (second par-tuple)
                                        :data-type (get-var-data-type (first par-tuple))
                                        :size (get-data-type-size (get-var-data-type (first par-tuple)))
                                        :pos idx}) (partition 2 par-vec))))
      
(defn register-function [symbols fn-name fn-label return-type parameters]
  (assoc symbols fn-name {:type :function
                          :name fn-name
                          :fn-label fn-label
                          :data-type return-type
                          :parameters (map-by-key :name parameters)}))

(defn register-parameter [symbols parameter stack-pos]
  (assoc symbols (parameter :name)
         {:type :parameter
          :data-type (parameter :data-type)
          :stack-pos stack-pos
          :size (parameter :size)}))

(defn generate-parameters [parameters symbols stack-info]
  "Returns [symbols par-init-code par-exit-code stack-info]"
  (if (zero? (count parameters))
    [symbols nil (asm :SET :PC, :POP) stack-info]
    (let [summed-par-size (reduce #(+ % (%2 :size)) 0 parameters)]
      (loop [parameters parameters
             symbols symbols
             stack-info stack-info
             stack-pos summed-par-size] 
        (if parameters
          (let [p (first parameters)
                symbols (register-parameter symbols p (inc stack-pos))]
            (recur (next parameters) symbols stack-info (- stack-pos (p :size))))
          [symbols nil (asm :SET :PC, :POP) stack-info])))))


(defn set-fn-result [result-info return-type]
  "Generates code for storing the function result in the output register
   reg A for 16 bit values
   reg A + B for 32 bit values"  
  (let [fn-result-info (convert-result-info result-info return-type)
        size (get-data-type-size return-type)
        [word1 word2] (get-result-info-word-output fn-result-info)]
    (when (not= :result-reg (fn-result-info :type))
      (cond 
        (= 1 size)
        (asm :SET :A, word1)
        (= 2 size)
        (concat (asm :SET :A, word1)
                (asm :SET :B, word2))
        :else
        (compiler-error (str "(set-fn-result) Unexpected size for result-info :" (result-info :size)))))))

(defn gen-push-regs 
  "Generates asm code for pushing all provided registers to the stack"
  [reg-usage]
  (mapcat #(asm :SET :PUSH, %) reg-usage))

(defn gen-pop-regs 
  "Generates asm code for popping all provided registers from the stack (in reversed order)"
  [reg-usage]
  (mapcat #(asm :SET %, :POP) (reverse reg-usage))) 

(defmethod parse-list 'defn [expr source symbols reg-info stack-info pref-result]
  "Syntax for defn:
   (defn return-type fn-name [parameters*] expression)"
  (let [[_ return-type-symbol fn-name parameter-vec expression] expr
         return-type (get-var-data-type return-type-symbol)
         parameters (extract-parameter-definitions parameter-vec)]
    (assert (symbol? fn-name) (str "defn: Function name is not a symbol: " fn-name))
    (assert (vector? parameter-vec) (str "defn: Expects a vector for the parameter definition instead of: " parameter-vec))
    (let [fn-label (keyword (str (gensym) \_ fn-name))
          symbols (register-function symbols fn-name fn-label return-type parameters)
          [fn-symbols par-init-code par-exit-code stack-info] (generate-parameters parameters symbols stack-info)
          [expr-mcode expr-fcode expr-dcode result-info reg-info] (parse-operand expression fn-symbols reg-info stack-info pref-result)
          reg-usage (determine-reg-usage expr-mcode)
          push-reg-code (gen-push-regs reg-usage)
          pop-reg-code (gen-pop-regs reg-usage)]
      [source
       nil
       (concat (asm fn-label)
               par-init-code
               (correct-sp-refs (concat push-reg-code 
                                        expr-mcode 
                                        (set-fn-result result-info return-type)
                                        pop-reg-code))
               par-exit-code
               expr-fcode)
       expr-dcode
       symbols 
       result-info])))

(defmethod parse-list 'set! [expr source symbols reg-info stack-info pref-result]
  "Syntax for set!:
   (set! var-name expression)"
  (let [[_ var-name expression] expr
        v (symbols var-name)]
    (assert (symbol var-name) (str "(set!) expects a variable symbol, but got: " var-name))
    (assert v (str "(set!) Variable '" var-name "' is not defined!"))
    (assert (v :is-mutable) (str "(set!) Variable '" var-name "' is not mutable!"))
    (assert (not (nil? expr)) (str "(set!) Expression is missing"))
    
    (let [[mcode fcode dcode result-info reg-info] (parse-operand expression symbols reg-info stack-info :result-reg)
          set-var-code (generate-var-set symbols var-name result-info stack-info reg-info)
          [unreserve-regs-code reg-info] (when (= :tmp-reg (result-info :type))
                                           [(post-reg-usage reg-info (result-info :loc) (result-info :loc2))
                                            (unreserve-regs reg-info (result-info :loc) (result-info :loc2))])]
      [source (concat mcode set-var-code unreserve-regs-code) fcode dcode symbols result-info])))

(defn get-fn-result-info [fn-sym]
  ; returns result-info
  ;TODO: Implement for different fn return types
  {:loc :A, :type :result-reg, :size 1 :data-type (fn-sym :data-type)})

(defn generate-fn-call [symdef expr source symbols reg-info stack-info]
  "Returns [next-source main-code func-code data-code symbols result-info]"
  (let [fn-params (symdef :parameters)
        fn-result-info (get-fn-result-info symdef)]
    (assert (= (count fn-params) (count expr)) (str "fn-call: Mismatching parameter count for function '" 
                                                    (symdef :name) "' - " (count fn-params) " parameter(s) expected, but " 
                                                    (count expr) " provided.")) 
    (loop [expressions expr
           mcode []
           fcode []
           dcode []
           par-stack-size 0
           result-info nil]
      (if expressions
        ;TODO: Check/Coerce parameter value types
        (let [[mexpr-code fexpr-code dexpr-code result-info reg-info] (parse-operand (first expressions) symbols reg-info stack-info :tmp-reg)]
          (recur (next expressions)
                 (concat mcode mexpr-code (asm :SET :PUSH, (result-info :loc)))
                 (concat fcode fexpr-code)
                 (concat dcode dexpr-code)
                 (+ par-stack-size (result-info :size))
                 result-info))
        [source (concat mcode 
                        (asm :JSR (symdef :fn-label)) 
                        (when (> par-stack-size 0) (asm :ADD :SP, par-stack-size))) fcode dcode symbols fn-result-info reg-info]))))   

(defn parse-symbol [sym expr source symbols reg-info stack-info pref-result]
  (let [symdef (symbols sym)]
    (assert symdef (str "Symbol is not defined: " sym))
    (case (symdef :type)
      :function
      (generate-fn-call symdef expr source symbols reg-info stack-info)
      :else
      (println "(parse-symbol) Not yet implemented: " (symdef :type)))))


(defmethod parse-list :default [expr source symbols reg-info stack-info pref-result]
  (if (symbol? (first expr))
    (parse-symbol (first expr) (next expr) source symbols reg-info stack-info pref-result)
    (println (str "Not yet implemented: " expr))))


(defn expr-dispatcher [expr source symbols stack-info]
  (cond
    (list? expr) :list
    :else :unknown))

(defmulti compile-expr expr-dispatcher)

(defmethod compile-expr :list [expr source symbols stack-info] 
  (parse-list expr source symbols initial-reg-info stack-info :result-reg))

(defmethod compile-expr :unknown [expr source symbols stack-info] 
  (throw (Exception. (str "Unknown expression: " expr))))

(defn compile-l16 [source-string]
  (loop [source (read-string (str \( source-string "\n)"))
         mcode-list []
         fcode-list []
         dcode-list []
         symbol-map {}]
    (if source
      (let [expr (first source)
            [rem-source add-mcode add-fcode add-dcode symbols _] (compile-expr expr (next source) symbol-map {:depth 0, :offsets []})]
        (recur rem-source
               (into mcode-list add-mcode)
               (into fcode-list add-fcode)
               (into dcode-list add-dcode)
               symbols))
      [(concat (asm :SET :SP, 0xFFFF) (correct-sp-refs mcode-list) (asm :SUB :PC, 1) fcode-list dcode-list) symbol-map])))

(defn do-compile [source-string & options]
 (let [code (first (process-labels (first (compile-l16 source-string)) 0))
       emu-options (set options)]
   (println "Generated ASM code:")
   (dump-asm (disasm code 0))
   (println "--------------------------------------------------------------------------")
   (println "Emulator run:")
   (if-bind (emu-options :org) [[mem regs cycles] 
                                (run-org (load-code initial-mem code 0) emu-options)
                                (run (load-code initial-mem code 0) emu-options)]
     (println "--------------------------------------------------------------------------")
     (println "Cycles     : " cycles)
     (println "Registers  : " regs)
     (println "Memory dump:")
     (dump-mem mem))))
   
  
