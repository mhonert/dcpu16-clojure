(in-ns 'dcpu16.emulator.core)

(def initial-mem (vec (repeat 65536 0)))

(defn reg-diff [old-regs new-regs]
  (apply hash-map (flatten (filter #(not= (% 1) (old-regs (% 0))) new-regs))))

(defn mem-diff [old-mem new-mem]
  (vec (mapcat #(if (= %1 %2) [0 0 0 0 0 0 0 0] %2) 
               (partition 8 8 [0 0 0 0 0 0 0 0] old-mem)
               (partition 8 8 [0 0 0 0 0 0 0 0] new-mem))))

(defn load-code [mem code pos]
  {:pre [(<= (+ pos (count code)) 65536)]}
  "Returns mem filled with the code at position pos"
  (vec (concat (subvec mem 0 pos) code (subvec mem (+ pos (count code))))))