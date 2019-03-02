(in-ns 'dcpu16.asm.core)

(defn define-label [label-map label address]
  (do
    (assert (nil? (label-map label)) (str "Label '" label "' was already defined!"))
    (assoc label-map label address)))


(defn reference-label [label-map [op-num label] address]
  (update-in label-map [label] #(conj % [address op-num])))

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

(defn calc-ref-addrs [op-num-addr-pairs]
  (map #(apply + %) op-num-addr-pairs))

(defn resolve-labels [src-code label-defs label-refs start-addr]
  (reduce (fn [dst-code label-kv]
            (let [label-addr (get-label-address label-defs (first label-kv))]
              (reduce (fn [dst-code ref-addr]
                        (update-in dst-code [ref-addr] #(+ % label-addr start-addr)))
                      dst-code (calc-ref-addrs (second label-kv)))))
          src-code label-refs))


(defn decode-operand [word code op-num]
  "returns [next-code op-value (or nil, if the operand cannot be optimized)]"
  (if-bind (= 1 op-num) [opcode (bit-and 0x1f (bit-shift-right word 5)) (bit-shift-right word 10)]
    (cond
      ; register
      (or (in-range? opcode 0x00 0x07)
          (= opcode 0x19)
          (in-range? opcode 0x18 0x1d))
      [code [opcode]]
      ; PUSH/POP
      (= opcode 0x18)
      [code nil]
      ; [SP + next word]
      (= opcode 0x1a)
      [(next code) [:SP (first code)]]
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
      [(next code) (first code)]
      :else ; literal value 0x00-0x1f
      [code nil])))

(defn optimize-label-ref-basic-opcode [code word label ref-addr ref-op]
  (let [sub-code (drop (inc ref-addr) code)
        [rest1 op2] (decode-operand word sub-code 2)
        [rest2 op1] (decode-operand word rest1 1)]
    (cond 
      ; op1 = register PC? / op2 = literal
      (and (number? op2) (vector? op1) (= (regs :PC) (first op1)))
      (let [diff (- op2 ref-addr)]
        (cond 
          (and (> diff 0) (<= diff 0x1f))
          [(vec (concat (take ref-addr code) (asm :ADD :PC, (dec diff)) rest2)) [ref-addr 1]]
          (and (<= diff 0) (>= diff -0x1d))
          [(vec (concat (take ref-addr code) (asm :SUB :PC, (- (dec diff))) rest2)) [ref-addr 1]]
          :else
          [(vec (concat (take ref-addr code) (asm :SET :PC, ref-addr) rest2)) [ref-addr 1]]))
      ; op1 = [SP + 0]
      (and (vector? op1) (= :SP (first op1)) (zero? (second op1)))
      [(vec (concat (take ref-addr code) 
                    (bit-or (bit-shift-left 0x19 5) (bit-and word 0xf81f)) 
                    (butlast rest1))) [ref-addr 1]]
        :else
      [code nil])))

(defn optimize-label-ref-non-basic-opcode [code word label ref-addr ref-op]
  (let [opcode (bit-and 0x1f (bit-shift-right word 5))]
    (if (non-basic-opcodes opcode)
      (let [sub-code (drop (inc ref-addr) code)
            [rest1 op1] (decode-operand word sub-code 2)]
        [code nil])
      (internal-error (str "(optimize-label-ref-non-basic-opcode) Unexpected non-basic-opcode: " opcode)))))

(defn optimize-label-ref [code [label ref-addr ref-op]]
   (let [word (code ref-addr)
         opcode (bit-and 0x001F word)]
     (cond 
       ; Non-basic opcode?
       (= 0 opcode)
       (optimize-label-ref-non-basic-opcode code word label ref-addr ref-op)
       ; Basic opcode?
       (opcodes opcode)
       (optimize-label-ref-basic-opcode code word label ref-addr ref-op)
       :else
       (internal-error (str "(optimize-label-ref) Unexpected opcode " opcode)))))


(defn optimize-1-label-ref [code label-defs label-refs]
  (let [refs (mapcat #(map (fn [s] (cons (first %) s)) (second %)) label-refs)]
    (loop [refs refs]
      (if refs
        (let [[code adjustment] (optimize-label-ref code (first refs))]
          (if adjustment
            [code adjustment]
            (recur (next refs))))
        [code nil]))))

(defn patch-operand [word code op-num offset adj-value ref-op]
  "returns [patched-code offset]"
  (let [adj-value adj-value];(if (= op-num ref-op) adj-value 0)] ; only adjust the address, if it is the correct operand
    (if-bind (= 1 op-num) [opcode 
                           (bit-and 0x1f (bit-shift-right word 5)) 
                           (bit-shift-right word 10)
                           patched-op-word 
                           (bit-or (bit-shift-left (- opcode adj-value) 5) (bit-and word 0xfc1f))
                           (bit-or (bit-shift-left (- opcode adj-value) 10) (bit-and word 0x03ff))]
    (cond
      ; register
      (or (in-range? opcode 0x00 0x07)
          (= opcode 0x19)
          (in-range? opcode 0x18 0x1d))
      [code offset]
      ; PUSH/POP
      (= opcode 0x18)
      [code offset]
      ; [SP + next word]
      (= opcode 0x1a)
      [code offset]
      ; [register]
      (in-range? opcode 0x08 0x0f)
      [code offset]
      ; [next word + register]
      (in-range? opcode 0x10 0x17)
      [(update-in code [(inc offset)] #(- % adj-value)) (inc offset)]
      ; [next word]
      (= opcode 0x1e)
      [(update-in code [(inc offset)] #(- % adj-value)) (inc offset)]
      ; next word (literal)
      (= opcode 0x1f)
      [(update-in code [(inc offset)] #(- % adj-value)) (inc offset)]
      :else ; literal value 0x00-0x1f
      [(assoc code 0 patched-op-word) offset]))))

(defn correct-label-basic-opcode [src-code word label ref-addr ref-op adj-value action]
  (let [pre-code (vec (drop ref-addr src-code))
        [code offset] (patch-operand word pre-code 2 0 adj-value ref-op)
        [code _] (patch-operand word code 1 offset adj-value ref-op)]
    ; op1 = register PC?
    (vec (concat (take ref-addr src-code) code))))

(defn correct-label-non-basic-opcode [src-code word label ref-addr op-num adj-value action]
  (let [opcode (bit-and 0x1f (bit-shift-right word 5))]
    (if (non-basic-opcodes opcode)
      (let [pre-code (vec (drop ref-addr src-code))
            [code offset] (patch-operand word pre-code 2 0 adj-value 2)]
        (vec (concat (take ref-addr src-code) code)))
      (internal-error (str "(correct-label-non-basic-opcode) Unexpected non-basic-opcode: " opcode)))))

(defn correct-label [code label-defs [label ref-addr op-num] adj-addr adj-value]
  (let [label-addr (label-defs label)
        pre-ref ref-addr
        [action ref-addr] (cond 
                 (and (<= label-addr adj-addr) (<= ref-addr adj-addr))
                 [:nothing ref-addr]
                 (and (> ref-addr adj-addr) (> label-addr adj-addr))
                 [:only-absolute-addr ref-addr]
                 :else
                 [:all ref-addr])]
    (if (not= action :nothing)
      (let [word (code ref-addr)
         opcode (bit-and 0x001F word)]
        (cond 
          ; Non-basic opcode?
          (= 0 opcode)
          (if (= action :all)
            (correct-label-non-basic-opcode code word label ref-addr op-num adj-value action)
            code)
          ; Basic opcode?
          (opcodes opcode)
          (if (or (= action :all) (#{(operations :SET)} opcode))
            (correct-label-basic-opcode code word label ref-addr op-num adj-value action)
            code)
          :else
          (internal-error (str "(correct-label) Unexpected opcode " opcode))))
      code)))
      
(defn correct-labels [code label-defs label-refs adjustment]
  "Returns [code label-defs label-refs]"
  ; Correct label definitions
  (let [[adj-addr adj-value] adjustment   
        label-defs (reduce #(assoc % (first %2) (if (> (second %2) adj-addr) (- (second %2) adj-value) (second %2))) 
                           {} label-defs)
        label-refs (apply hash-map 
                          (mapcat (fn [[k v]] 
                                    [k (map (fn [[addr op-num]] 
                                              [(if (> addr adj-addr) 
                                                 (- addr adj-value) 
                                                 addr) op-num]) v)]) label-refs))
        code (loop [refs (mapcat #(map (fn [s] (cons (first %) s)) (second %)) label-refs)
                    code code]
               (if refs
                 (let [code (correct-label code label-defs (first refs) adj-addr adj-value)]
                   (recur (next refs) code))
                 code))]
    [code label-defs label-refs]))

(defn optimize-labels [code label-defs label-refs]
  (if (empty? label-refs)
    [code label-defs label-refs]
    (loop [code code
           label-defs label-defs
           label-refs label-refs]
      (let [[code adjustment] (optimize-1-label-ref code label-defs label-refs)]
        (if adjustment
          (let [[code label-defs label-refs] (correct-labels code label-defs label-refs adjustment)]
            (recur code label-defs label-refs))
          [code label-defs label-refs])))))

(defn process-labels [code start-addr]
  (let [[code label-defs label-refs] (extract-labels code)]
    (optimize-labels (resolve-labels code label-defs label-refs start-addr) label-defs label-refs)))
  
  

  
