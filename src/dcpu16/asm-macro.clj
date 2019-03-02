(ns dcpu16.asm-macro (:use [dcpu16.emit] [dcpu16.util]))

(def regkeywords {'A :A, 'B :B, 'C :C, 'X :X
           'Y :Y, 'Z :Z, 'I :I, 'J :J
           'SP :SP, 'PC :PC, 'O :O
           'POP :POP 'PEEK :PEEK 'PUSH :PUSH})

(def opkeywords 
  {'SET :SET, 'ADD :ADD, 'SUB :SUB, 'MUL :MUL
   'DIV :DIV, 'MOD :MOD, 'SHL :SHL, 'SHR :SHR
   'AND :AND, 'BOR :BOR, 'XOR :XOR, 'IFE :IFE
   'IFN :IFN, 'IFG :IFG, 'IFB :IFB})

(def non-basic-opkeywords
  {'JSR :JSR})

(defn syntax-error [msg]
  (throw (Exception. msg)))

(defn read-mem-label-reg-operand [label reg [ntoken nntoken & exprs]]
  (cond 
    ; [:LABEL + REG + ...]
    (= '+ ntoken)
    (if (number? nntoken) ;[:LABEL + REG + LITERAL]
      [[`(memreglitop ~reg ~nntoken) label]]
      (syntax-error (str "(read-mem-label-reg-operand) Unexpected token: " nntoken)))
    ; [:LABEL + REG]
    (nil? ntoken)
    [[`(memreglitop ~reg 0) label]]
    :else (syntax-error (str "(read-mem-label-reg-operand) Unexpected token: " ntoken))))
      
(defn read-mem-label-operand [label ntoken exprs]
  (let [nntoken (first exprs)]
    (if (= '+ ntoken)
      ; [:LABEL + ...]
      (cond
        ;[:LABEL + REG ...
        (regkeywords nntoken)
        (read-mem-label-reg-operand label (regkeywords nntoken) (next exprs))
        ;[:LABEL + LITERAL]
        (number? nntoken)
        [[`(memlitop ~nntoken) label]]
        :else (syntax-error (str "(read-mem-label-operand) Unexpected token: " nntoken)))
      ; [:LABEL]
      [[`(memlitop 0) label]])))
        
(defn read-mem-operand [[token ntoken & tokens]]
 (cond
   ; [:REG ...]
   (regkeywords token) (if (= '+ ntoken)
                         [[`(memreglitop ~(regkeywords token) ~(first tokens))]]
                         [[`(memregop ~(regkeywords token))]])
   ; [:LABEL ...]
   (symbol? token)
   (read-mem-label-operand (keyword token) ntoken tokens)
   ; [:LITERAL ...]
   (number? token)
   (if (= '+ ntoken)
     ; [:LITERAL + ...]
     [[`(memreglitop ~(regkeywords (first tokens)) ~token)]]
     ; [:LITERAL]
     [[`(memlitop ~token)]])
   :else (syntax-error (str "Unexpected token: " token))))
        
(defn read-label-operand [label exprs]
  (let [ntoken (first exprs)
        nntoken (second exprs)]
    (if (= '+ ntoken)
      ; :LABEL + ...
      (cond       
        ; : LABEL + LITERAL
        (number? nntoken)
        [[`(litop ~nntoken :no-optimization) label] (nnext exprs)]
        :else (syntax-error (str "Unexpected token: " nntoken)))
      ; :LABEL
      [[`(litop 0 :no-optimization) label] exprs])))

(defn read-1-operand [exprs]
  "Reads 1 operand from the given list of tokens"
   (let [token (first exprs)
         rest-1 (next exprs)]
     (cond
       (regkeywords token) [[`(regop ~(regkeywords token))] rest-1]
       (vector? token) (conj (read-mem-operand token) rest-1)
       (symbol? token) (read-label-operand (keyword token) rest-1)
       (number? token) [[`(litop ~token)] rest-1]
       :else (syntax-error (str "Unexpected token: " (type token))))))

(defn read-2-operands [exprs]
  "Reads 2 operands from the given list of tokens"
  (let [result1 (read-1-operand exprs)
        result2 (read-1-operand (second result1))]
    [(first result1) (first result2) (second result2)])) 

(defn read-data [[data & exprs]]
  "Reads a data definition.
   Syntax: DAT [data]
   Example: DAT [1 2 3 4 \"Test\""
  (if (vector? data)
    [[(vec (mapcat 
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
           (not= (int %) %)
           (do
             (assert (and (>= (int %) -32768) (<= (int %) 32767)) "Fixed point number is < -32768 or > 32767")
             (if-bind (< % 0) [v (+ % 65536) %]
                      [(int v) (Math/abs (int (* 65536 (- (int %) %))))]))
           ; 32 Bit?
           (or (< % -32768) (> % 65535))
           (do
             (assert (and (>= % -2147483648) (<= % 4294967295)) "32-Bit number is < -2147483648 or > 4294967295")
             (if-bind (< % 0) [v (+ % 4294967296) %]
                      [(bit-shift-right v 16) (bit-and v 0x0000FFFF)]))
           ; 16 Bit
           :else
           (if-bind (< % 0) [v (+ % 65536) %]
                    [v]))
         :else
         (syntax-error (str "Unexpected data element: " %)))
      data))] exprs]
    (syntax-error (str "DAT expects a vector of data element, instead of: " data))))

(defn read-operation [[op & exprs]]
  "Reads an operation including its operands from the given list of tokens"
  (when op
    ; Operation with 2 operands?
    (if-let [opkeyword (opkeywords op)]
      (let [[[op1 & [label1]] [op2 & [label2]] rest-exprs] (read-2-operands exprs)]
        (cond
          (and label1 label2)
          [[[[`(dec (count ~op1)) label1]] [`(+ (dec (count ~op1)) (dec (count ~op2))) label2] `(demit ~opkeyword ~op1 ~op2)] rest-exprs]
          label1
          [[[[`(dec (count ~op1)) label1]] `(demit ~opkeyword ~op1 ~op2)] rest-exprs]
          label2
          [[[[`(+ (dec (count ~op1)) (dec (count ~op2))) label2]] `(demit ~opkeyword ~op1 ~op2)] rest-exprs]
          :else
          [[`(demit ~opkeyword ~op1 ~op2)] rest-exprs]))
      ; Operation with 1 operand?
      (if-let [opkeyword (non-basic-opkeywords op)]
        (let [[[op1 & [label1]] rest-exprs] (read-1-operand exprs)]
          (if label1
            [[[[`(dec (count ~op1)) label1]] `(demit ~opkeyword ~op1)] rest-exprs]
            [[`(demit ~opkeyword ~op1)] rest-exprs]))
        (cond 
          ; Label?
          (keyword? op)
          [[[op]] exprs]
          ; DAT ?
          (= 'DAT op)
          (read-data exprs)
          :else
          (syntax-error (str "Unknown operation: " op " / expr: " exprs)))))))
  
(defn read-operations [exprs result-list]
  (if (empty? exprs)
    result-list
    (let [[op-expr rest-exprs] (read-operation exprs)]
      (recur rest-exprs (into result-list op-expr)))))

(defmacro $ [& exprs]
  (when (seq exprs)
    `(concat ~@(read-operations exprs []))))

(defn define-label [label-map label address]
  (do
    (assert (nil? (label-map label)) (str "Label '" label "' was already defined!"))
    (assoc label-map label address)))


(defn reference-label [label-map [operand-num label] address]
  (update-in label-map [label] #(conj % (+ address operand-num))))

(defn extract-labels [code]
  (loop [src-code (next code)
         dst-code []
         item (first code)
         address 0
         label-defs {}
         label-refs {}]
    (cond 
      ; label definition?
      (keyword? item)
      (recur (next src-code)
             dst-code
             (first src-code)
             address
             (define-label label-defs item address)
             label-refs)
      ; label reference?
    (vector? item)
      (recur (next src-code)
             dst-code
             (first src-code)
             address
             label-defs
             (reference-label label-refs item address))
      ; opcode or data?
      (number? item)
      (recur (next src-code)
             (conj dst-code item)
           (first src-code)
           (inc address)
           label-defs
           label-refs)
      ; end of code?
      (nil? item)
      [dst-code label-defs label-refs]
      :else
      (throw (Exception. (str "Unexpected item: " item))))))

(defn get-label-address [label-map label]
  (do
    (assert (label-map label) (str "Label '" label "' is not defined!"))
    (label-map label)))

(defn resolve-labels [[src-code label-defs label-refs] start-addr]
  (reduce (fn [dst-code label-kv]
            (let [label-addr (get-label-address label-defs (first label-kv))]
              (reduce (fn [dst-code ref-addr]
                        (update-in dst-code [ref-addr] #(+ % label-addr start-addr)))
                      dst-code (second label-kv))))
          src-code label-refs))

(defn process-labels [code start-addr]
  (resolve-labels (extract-labels code) start-addr))
  
  
  