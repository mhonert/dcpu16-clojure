(ns dcpu16.util)


(defn get-bindings-for-true [[bind-symbol bind-val-true bind-val-false & more-bindings]]
  (when bind-symbol
    (concat [bind-symbol bind-val-true] (get-bindings-for-true more-bindings))))
 
(defn get-bindings-for-false [[bind-symbol bind-val-true bind-val-false & more-bindings]]
  (when bind-symbol
    (concat [bind-symbol bind-val-false] (get-bindings-for-false more-bindings))))

(defmacro if-bind [condition [& bindings] & exprs]
  "bindings: [bind-symbol bind-val-true bind-val-false & more-bindings]
   Binds the bind-symbol either to bind-val-true or bind-val-false depending upon
   the result of the condition and evaluates the exprs in the lexical context of
   this binding."
  `(if ~condition
     (let [~@(get-bindings-for-true bindings)]
       ~@exprs)
     (let [~@(get-bindings-for-false bindings)]
       ~@exprs)))

(defmacro in-range? [value start end]
  "Checks, whether the value is within the given boundaries
   (boundaries include start and end values)"
  `(let [v# ~value]
     (and (>= v# ~start) (<= v# ~end))))

(defn map-by-key [k s]
   "Creates a new map from a sequence of maps, which all share a common key (k)
    The values of this key will be used as the key for the new map:
    e.g.:
    s => [{:id 1000 :value ""test1""} {:id 1001 :value ""test2""}]
    k => :id
    Result: {1000 {:id 1000 value ""test1""}, 1001 {:id 1001 :value ""test2""}}"
   (reduce #(into % {(%2 k) %2}) {} s))

(defn third [s]
  "Returns the third element of the sequence"
  (second (next s)))

(defn nnnext [s]
  (next (nnext s)))

(defn hex [word]
  (format "%04x" word))

(defn to-2s-complement
  "Converts the provided value into 2's-complement representation"
  [v]
  (if (< v 0) (+ 65536 v) v))

(defn from-2s-complement
  "Converts the provided 2's complement value to a signed integer value"
  [v]
  (if (> v 32767) (- v 65536) v))

(defn sgn
  "Returns -1 if the provided value is negative or 1 if it is positive
   (for value zero, 1 will be returned)"
  [v]
  (if (< v 0) -1 1))

(defn internal-error [msg]
  (throw (Exception. (str "Internal error: " msg))))