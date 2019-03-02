(in-ns 'dcpu16.compiler.core)

(def managed-regs #{:C :X :Y :Z})

(def initial-reg-info {:free-regs [:C :X :Y :Z] :usage-count {:C 0 :X 0 :Y 0 :Z 0}})

(defn use-reg [reg-info reg]
  "Increases the usage counter for the register"
  (update-in reg-info [:usage-count reg] inc))

(defn unuse-reg [reg-info reg]
  "Decreases the usage counter for the register"
  (update-in reg-info [:usage-count reg] dec))

(defn pre-reg-usage [reg-info & regs]
  "Returns code that must be emitted before using any of the given registers"
  (mapcat #(when (and (managed-regs %) (> (get-in reg-info [:usage-count %]) 1))
             (asm :SET :PUSH, %))
          regs))

(defn post-reg-usage [reg-info & regs]
  "Returns code that must be emitted after the use of the given registers"
  (mapcat #(when (and (managed-regs %) (> (get-in reg-info [:usage-count %]) 1))
             (asm :SET %, :POP)) 
          (reverse regs)))

(defn get-free-reg [reg-info]
  "Returns [reg-info reg] (does not check, whether there are any free registers left!)"
  (let [reg (first (reg-info :free-regs))]
    [(update-in reg-info [:free-regs] next) reg]))
  
(defn reserve-reg [reg-info]
  "Reserves a register and returns [reg reg-info]"
  (if-bind (> (count (reg-info :free-regs)) 0) [[reg-info reg] (get-free-reg reg-info) [reg-info :C]]
    [(use-reg reg-info reg) reg]))

(defn reserve-2-regs [reg-info]
  "Reserves two registers and returns [reg1 reg2 reg-info]"
  (condp <= (count (reg-info :free-regs))
    2 (let [[reg1 reg-info] (get-free-reg reg-info)
            [reg2 reg-info] (get-free-reg reg-info)]
        [(use-reg (use-reg reg-info reg1) reg2) reg1 reg2 ]
    1 (reserve-reg reg-info)
    0 [(use-reg (use-reg reg-info :C) :X) :C :X])))

(defn unreserve-reg [reg-info reg]
  "Unreserves a register and returns the updated reg-info"
  (if (managed-regs reg)
    (let [reg-info (unuse-reg reg-info reg)]
      (if (zero? (get-in reg-info [:usage-count reg]))
        (update-in reg-info [:free-regs] #(cons reg %))
        reg-info))
    reg-info))

(defn unreserve-regs [reg-info & regs]
  (loop [regs regs
         reg-info reg-info]
    (if regs
      (recur (next regs)
             (unreserve-reg reg-info (first regs)))
      reg-info)))
