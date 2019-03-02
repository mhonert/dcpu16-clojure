(in-ns 'dcpu16.asm.core)

(def regkeywords #{:A, :B, :C , :X
                   :Y, :Z, :I, :J
                   :SP, :PC, :EX
                   :PUSH, :POP, :PEEK})

(def opkeywords #{:SET, :ADD, :SUB, :MUL, :MLI, :DIV, :DVI, :MOD
                  :MDI, :AND, :BOR, :XOR, :SHR, :ASR, :SHL, :IFB
                  :IFC, :IFE, :IFN, :IFG, :IFA, :IFL, :IFU, :ADX
                  :SBX, :STI, :STD})

(def non-basic-opkeywords #{:JSR, :HCF, :INT, :IAG, :IAS, :RFI, :IAQ, :HWN, :HWQ, :HWI})

(def regopcodes {0x00 "A", 0x01 "B", 0x02 "C", 0x03 "X"
                 0x04 "Y", 0x05 "Z", 0x06 "I", 0x07 "J"
                 0x1b "SP", 0x1c "PC", 0x1d "EX"
                 0x18 "PUSHPOP", 0x19 "PEEK"})

(def opcodes 
  {0x01 "SET", 0x02 "ADD", 0x03 "SUB", 0x04 "MUL"
   0x05 "MLI", 0x06 "DIV", 0x07 "DVI", 0x08 "MOD"
   0x09 "MDI", 0x0A "AND", 0x0B "BOR", 0x0C "XOR"
   0x0D "SHR", 0x0E "ASR", 0x0F "SHL", 0x10 "IFB"
   0x11 "IFC", 0x12 "IFE", 0x13 "IFN", 0x14 "IFG"
   0x15 "IFA", 0x16 "IFL", 0x17 "IFU", 0x1a "ADX"
   0x1b "SBX", 0x1e "STI", 0x1f "STD"})
 
(def non-basic-opcodes
  {0x01 "JSR", 0x07 "HCF", 0x08 "INT", 0x09 "IAG"
   0x0a "IAS", 0x0b "RFI", 0x0c "IAQ"
   0x10 "HWN", 0x11 "HWQ", 0x12 "HWI"})

(def regs {:A 0x00, :B 0x01, :C 0x02 , :X 0x03
           :Y 0x04, :Z 0x05, :I 0x06 , :J 0x07
           :SP 0x1b, :PC 0x1c, :EX 0x1d
           :PUSH 0x18, :POP 0x18, :PEEK 0x19})

(def allowed-mem-regs #{:A :B :C :X :Y :Z :I :J :SP})

(def operations 
  {:SET 0x01, :ADD 0x02, :SUB 0x03, :MUL 0x04
   :MLI 0x05, :DIV 0x06, :DVI 0x07, :MOD 0x08
   :MDI 0x09, :AND 0x0A, :BOR 0x0B, :XOR 0x0C
   :SHR 0x0D, :ASR 0x0E, :SHL 0x0F, :IFB 0x10
   :IFC 0x11, :IFE 0x12, :IFN 0x13, :IFG 0x14
   :IFA 0x15, :IFL 0x16, :IFU 0x17, :ADX 0x1a
   :SBX 0x1b, :STI 0x1e, :STD 0x1f})

(def non-basic-operations
  {:JSR 0x01, :HCF 0x07, :INT 0x08, :IAG 0x09
   :IAS 0x0a, :RFI 0x0b, :IAQ 0x0c
   :HWN 0x10, :HWQ 0x11, :HWI 0x12})

(def op-cycles 
  {:SET 1, :ADD 2, :SUB 2, :MUL 2
   :MLI 2, :DIV 3, :DVI 3, :MOD 3
   :MDI 3, :AND 1, :BOR 1, :XOR 1
   :SHR 1, :ASR 1, :SHL 1, :IFB 2
   :IFC 2, :IFE 2, :IFN 2, :IFG 2
   :IFA 2, :IFL 2, :IFU 2, :ADX 3
   :SBX 3, :STI 2, :STD 2
   :JSR 3, :HCF 9, :INT 4, :IAG 1
   :IAS 1, :RFI 3, :IAQ 2
   :HWN 2, :HWQ 4, :HWI 4})

(def special-cycles
  {:IFSKIP 1})

(def op-access-cycles
  {:REG 0
   :REGMEM 0
   :REGLITMEM 1
   :PUSHPOP 0
   :PEEK 0
   :LITMEM 1
   :LIT 1
   :SHORTLIT 0})
   
  
  
  
  