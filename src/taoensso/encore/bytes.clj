(ns ^:no-doc taoensso.encore.bytes
  "Experimental, subject to change without notice!!
  Private low-level byte[] utils."
  {:added "v3.69.0 (2023-10-16)"}
  (:refer-clojure :exclude [bytes?])
  (:require [taoensso.encore :as enc :refer [have have?]])
  (:import
   [java.nio.charset StandardCharsets]
   [java.util BitSet]
   [java.io
    DataInput  DataInputStream
    DataOutput DataOutputStream
    ByteArrayInputStream
    ByteArrayOutputStream]))

(comment
  (remove-ns 'taoensso.encore.bytes)
  (:api (enc/interns-overview)))

;;;; Unsigned ints

(do
  (def ^:const range-ubyte  "Max unsigned byte: 255"          (-    Byte/MAX_VALUE    Byte/MIN_VALUE))
  (def ^:const range-ushort "Max unsigned short: 65,535"      (-   Short/MAX_VALUE   Short/MIN_VALUE))
  (def ^:const range-uint   "Max unsigned int: 4,294,967,295" (- Integer/MAX_VALUE Integer/MIN_VALUE))

  (let [fail!
        (fn [n target-type target-min target-max]
          (throw
            (ex-info "Failed to cast number to typed int (exceeds type range)"
              {:given  {:value n :type (type n)}
               :target {:type target-type :min target-min :max target-max}})))]

    (defn as-byte   ^long [^long n] (if (and (>= n    Byte/MIN_VALUE) (<= n    Byte/MAX_VALUE)) n (fail! n :byte     Byte/MIN_VALUE    Byte/MAX_VALUE)))
    (defn as-short  ^long [^long n] (if (and (>= n   Short/MIN_VALUE) (<= n   Short/MAX_VALUE)) n (fail! n :short   Short/MIN_VALUE   Short/MAX_VALUE)))
    (defn as-int    ^long [^long n] (if (and (>= n Integer/MIN_VALUE) (<= n Integer/MAX_VALUE)) n (fail! n :int   Integer/MIN_VALUE Integer/MAX_VALUE)))

    (defn as-ubyte  ^long [^long n] (if (and (>= n 0) (<= n range-ubyte))  n (fail! n :ubyte  0 range-ubyte)))
    (defn as-ushort ^long [^long n] (if (and (>= n 0) (<= n range-ushort)) n (fail! n :ushort 0 range-ushort)))
    (defn as-uint   ^long [^long n] (if (and (>= n 0) (<= n range-uint))   n (fail! n :uint   0 range-uint))))

  (defn to-ubyte        "Signed ℤ[-128,127] -> unsigned ℕ[0,255]"   ^long [^long n] (as-ubyte  (- n    Byte/MIN_VALUE)))
  (defn to-ushort   "Signed ℤ[-32768 32767] -> unsigned ℕ[0,65535]" ^long [^long n] (as-ushort (- n   Short/MIN_VALUE)))
  (defn to-uint                   "Signed ℤ -> unsigned ℕ[0,max]"   ^long [^long n] (as-uint   (- n Integer/MIN_VALUE)))

  (defn from-ubyte    "Unsigned ℕ[0,255] -> signed ℤ[-128,127]"     ^long [^long n] (+ (as-ubyte  n)    Byte/MIN_VALUE))
  (defn from-ushort "Unsigned ℕ[0,65535] -> signed ℤ[-32768,32767]" ^long [^long n] (+ (as-ushort n)   Short/MIN_VALUE))
  (defn from-uint     "Unsigned ℕ[0,max] -> signed ℤ"               ^long [^long n] (+ (as-uint   n) Integer/MIN_VALUE))

  (defn  byte->ba ^bytes [n] (let [bb (java.nio.ByteBuffer/allocate             1)] (.put      bb (byte  (as-byte  n))) (.array bb)))
  (defn short->ba ^bytes [n] (let [bb (java.nio.ByteBuffer/allocate   Short/BYTES)] (.putShort bb (short (as-short n))) (.array bb)))
  (defn   int->ba ^bytes [n] (let [bb (java.nio.ByteBuffer/allocate Integer/BYTES)] (.putInt   bb (int   (as-int   n))) (.array bb)))

  (defn ba->byte  [^bytes ba] (aget ba 0))
  (defn ba->short [^bytes ba] (let [bb (java.nio.ByteBuffer/allocate   Short/BYTES)] (.put bb ba) (.flip bb) (.getShort bb)))
  (defn ba->int   [^bytes ba] (let [bb (java.nio.ByteBuffer/allocate Integer/BYTES)] (.put bb ba) (.flip bb) (.getInt   bb)))

  (defn write-ubyte  "Writes unsigned byte  as 1 signed byte"  [^DataOutput out ubyte]  (.writeByte  out (from-ubyte  ubyte)))
  (defn write-ushort "Writes unsigned short as 2 signed bytes" [^DataOutput out ushort] (.writeShort out (from-ushort ushort)))
  (defn write-uint   "Writes unsigned int   as 4 signed bytes" [^DataOutput out uint]   (.writeInt   out (from-uint   uint)))

  (defn read-ubyte   "Reads unsigned byte  from 1 signed byte"  ^long [^DataInput in] (to-ubyte  (.readByte  in)))
  (defn read-ushort  "Reads unsigned short from 2 signed bytes" ^long [^DataInput in] (to-ushort (.readShort in)))
  (defn read-uint    "Reads unsigned int   from 4 signed bytes" ^long [^DataInput in] (to-uint   (.readInt   in))))

(comment
  (enc/qb 1e6 ; [66.26 68.72 70.05]
    (ba->byte  (byte->ba  25))
    (ba->short (short->ba 25))
    (ba->int   (int->ba   25))))

;;;; Basics

(defn n-bytes->n-bits ^long [n-bytes] (*    (int n-bytes) 8))
(defn n-bits->n-bytes ^long [n-bits]  (quot (int n-bits)  8))

(enc/defaliases
  enc/bytes-class
  enc/bytes?
  enc/ba=
  enc/ba-hash

  enc/utf8-ba->str
  enc/str->utf8-ba

  enc/const-ba=)

(defmacro ba0 [] `(byte-array 0))
(defn ?ba-len ^long [?ba] (if ?ba (alength ^bytes ?ba) 0))
(defn ?ba= [?ba1 ?ba2]
  (if-let [^bytes ba1 ?ba1]
    (if-let [^bytes ba2 ?ba2]
      (java.util.Arrays/equals ba1 ba2)
      (zero? (alength ba1)))

    (if-let [^bytes ba2 ?ba2]
      (zero? (alength ba2))
      true)))

(defn ba-join*
  "Returns byte[] concatenation of >= 0 ?byte[]s."
  ^bytes [?bas]
  (let [total-len (reduce (fn [^long acc in] (if in (+ acc (alength ^bytes in)) acc)) 0 ?bas)
        out       (byte-array total-len)]
    (loop [idx 0, remaining ?bas]
      (if-let [[in] remaining]
        (if in
          (let [len (alength ^bytes in)]
            (do
              (System/arraycopy in 0 out idx len)
              (recur (+ idx len) (next remaining))))
          (recur idx (next remaining)))
        out))))

(let [ba0 (byte-array 0)]
  (defn ba-join
    "Returns byte[] concatenation of >= 0 ?byte[]s."
    (^bytes [         ]         ba0)
    (^bytes [?ba      ] (or ?ba ba0))
    (^bytes [?ba1 ?ba2]
     (enc/cond
       (nil? ?ba1) (or ?ba2 ba0)
       (nil? ?ba2) (or ?ba1 ba0)
       :else
       (let [^bytes ba1 ?ba1
             ^bytes ba2 ?ba2
             l1  (alength ba1)
             l2  (alength ba2)
             out (byte-array (+ l1 l2))]
         (System/arraycopy ba1 0 out 0  l1)
         (System/arraycopy ba2 0 out l1 l2)
         (do                     out))))

    (^bytes [?ba1 ?ba2 & more]
     (ba-join (ba-join ?ba1 ?ba2) (ba-join* more)))))

(comment (vec (let [ba byte-array] (ba-join nil (ba [0]) (ba [1 2]) nil (ba [3 4 5]) nil nil (ba [6])))))

(let [ba-range
      (fn [^bytes ba ^long lidx ^long ridx]
        (when (pos? (- ridx lidx))
          (java.util.Arrays/copyOfRange ba lidx ridx)))]

  (defn ba-parts*
    "Returns `to` with partitions of given byte[] coinjoined."
    [to ba start-idx lengths]
    (loop [idx (long start-idx), remaining lengths, acc to]
      (if-let [[^int len] remaining]
        (recur (+ idx len) (next remaining)
          (conj acc (ba-range ba idx (+ idx len))))
        (conj   acc (ba-range ba idx (alength ^bytes ba))))))

  (defn ba-parts
    "Returns vector of partitions of given byte[]."
    ([ba start-idx len1] ; => [<len1><rest>]
     (let [idx2 (+ ^int start-idx ^int len1)]
       [(ba-range ba start-idx idx2)
        (ba-range ba idx2 (alength ^bytes ba))]))

    ([ba start-idx len1 len2] ; => [<len1><len2><rest>]
     (let [idx2 (+ ^int start-idx ^int len1)
           idx3 (+      idx2      ^int len2)]
       [(ba-range ba start-idx idx2)
        (ba-range ba idx2      idx3)
        (ba-range ba idx3 (alength ^bytes ba))]))

    ([ba start-idx len1 len2 & more] ; => [<len1><len2><...><rest>]
     (let [idx2 (+ ^int start-idx ^int len1)
           idx3 (+      idx2      ^int len2)
           acc
           [(ba-range ba start-idx idx2)
            (ba-range ba idx2      idx3)]]

       (ba-parts* acc ba idx3 more)))))

(comment (mapv vec (ba-parts (byte-array (range 16)) 0 1 2 3 4)))

(defn ba->len
  "Returns given byte[] `ba` if its length exactly matches `target-len`.
  Otherwise returns a truncated or zero-padded copy of `ba` as necessary."
  ^bytes [target-len ^bytes ba]
  (let [target-len (int target-len)]
    (if (== target-len (alength ba))
      ba
      (java.util.Arrays/copyOf ba target-len))))

(defn ba->sublen
  "Like `ba->len`, but will throw instead of padding."
  ^bytes [target-len ^bytes ba]
  (let [actual-len (alength ba)
        target-len (int target-len)]
    (enc/cond
      (== target-len actual-len) ba
      (<  target-len actual-len) (java.util.Arrays/copyOf ba target-len)
      :else
      (throw
        (ex-info "Given byte[] too short"
          {:length {:actual actual-len, :target target-len}})))))

;;;; To/from strings
;; Java strings are UTF-16, but we'll use UTF-8 encoding when converting to/from bytes

(def ^:const utf8-str "hello ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸ world")
(defn ?utf8-ba->?str [?ba] (when-let [ba ?ba] (enc/utf8-ba->str ba)))
(defn ?str->?utf8-ba [?s]  (when-let [s  ?s]  (enc/str->utf8-ba  s)))
(declare ca->utf8-ba chars?)

(defn ^:public as-ba
  "Returns a byte[] from given input:
    byte[]   -> byte[]
    string[] -> corresponding UTF-8 byte[]
    char[]   -> corresponding UTF-8 byte[]
    int      -> (byte-array <int>)
    seqable  -> (byte-array <seq>)

  When `target-len` is provided, trim or zero-pad the returned array as necessary."
  (^bytes [target-len x] (ba->len target-len (as-ba x)))
  (^bytes [           x]
   (enc/cond
     (bytes?   x)               x
     (string?  x) (str->utf8-ba x)
     (chars?   x) (ca->utf8-ba  x)
     (enc/int? x) (byte-array   x)
     (seqable? x) (byte-array   x)
     :else
     (enc/unexpected-arg! x
       {:context  `as-ba
        :expected '#{byte-array string char-array int seqable}}))))

(comment (vec (as-ba 16 "hello")))

(defn as-?ba
  ([target-len ?x] (when-let [x ?x] (as-ba target-len x)))
  ([           ?x] (when-let [x ?x] (as-ba            x))))

#_
(defn as-str ^String [x]
  (enc/cond
    (string? x)                        x
    (bytes?  x) (enc/utf8-ba->str      x)
    (chars?  x) (String/valueOf ^chars x)
    :else       (str                   x)))

#_(defn as-?str [x] (when x (as-str x)))

;;;; To/from char[]s

(let [c (Class/forName "[C")] (defn chars? [x] (instance? c x)))

(defn as-ca
  "If given a char[]: returns the char[].
  If given a string or byte[]: returns the corresponding char[]."
  ^chars [x]
  (enc/cond
    (chars?  x)                                     x
    (string? x) (.toCharArray ^String               x)
    (bytes?  x) (.toCharArray ^String (utf8-ba->str x))
    :else
    (enc/unexpected-arg! x
      {:context  `as-ca
       :expected '#{char-array string byte-array}})))

;;;; Byte streams

(defn with-out* ^bytes [buffer-len out-fn]
  (let [baos (ByteArrayOutputStream. buffer-len)
        out  (DataOutputStream. baos)]
    (out-fn out   baos)
    (.toByteArray baos)))

(defn with-in* [^bytes ba in-fn]
  (let [bais (ByteArrayInputStream. ba)
        in   (DataInputStream. bais)]
    (in-fn in bais)))

(defn ^:no-doc parse-buffer-len
  "Private, implementation detail."
  [spec]
  (reduce
    (fn [^long acc in]
      (if in
        (if (bytes? in)
          (+ acc (alength ^bytes in))
          (+ acc (long           in)))
        acc))
    0 spec))

(defmacro with-out
  [[dos-sym ?baos-sym] buffer-len & body]
  (let [baos-sym (or ?baos-sym '__baos)
        buffer-len
        (if (vector? buffer-len)
          `(parse-buffer-len ~buffer-len)
          (do                 buffer-len))]

    `(with-out* ~buffer-len
       (fn [~(with-meta  dos-sym {:tag 'java.io.DataOutputStream})
            ~(with-meta baos-sym {:tag 'java.io.ByteArrayOutputStream})]
         ~@body))))

(defmacro with-in
  [[din-sym ?bais-sym] ba & body]
  (let [bais-sym (or ?bais-sym '__bais)]
    `(with-in* ~ba
       (fn [~(with-meta din-sym  {:tag 'java.io.DataInput})
            ~(with-meta bais-sym {:tag 'java.io.ByteArrayInputStream})]
         ~@body))))

;;;;

(defn write-dynamic-uint
  "Writes given unsigned int to `out` as 1-5 bytes.
  Returns the number of bytes written."
  ^long [^DataOutput out unsigned-int]
  (let [n (long unsigned-int)]
    ;; [-128,124] used for [0,252] ; (from-ubyte 252)
    ;; [125, 127] used to indicate typed size prefix
    (enc/cond
      (<= n          252) (do                      (.writeByte  out (from-ubyte  n)) 1) ; 1     byte  for [0,252]
      (<= n  range-ubyte) (do (.writeByte out 125) (.writeByte  out (from-ubyte  n)) 2) ; 1+1=2 bytes for [0,255]
      (<= n range-ushort) (do (.writeByte out 126) (.writeShort out (from-ushort n)) 3) ; 1+2=3 bytes for [0,65535]
      (<= n   range-uint) (do (.writeByte out 127) (.writeInt   out (from-uint   n)) 5) ; 1+4=5 bytes for [0,4294967295]
      :else
      (throw
        (ex-info "Dynamic unsigned int exceeds max"
          {:value n, :max range-uint})))))

(defn read-dynamic-uint
  "Reads 1-5 bytes from `in`, and returns unsigned int."
  ^long [^DataInput in]
  (let [b1 (.readByte in)]
    (case b1
      127 (to-uint   (.readInt   in)) ; 1+4=5 bytes for [0,4294967295]
      126 (to-ushort (.readShort in)) ; 1+2=3 bytes for [0,65535]
      125 (to-ubyte  (.readByte  in)) ; 1+1=2 bytes for [0,255]
      (do (to-ubyte              b1)) ; 1     byte  for [0,252]
      )))

(defn write-dynamic-ba
  "Writes possible byte[] `?ba` to `out` as 1-5 bytes.
  Returns the number of bytes written."
  ^long [^DataOutput out ?ba]
  (if-let [^bytes ba ?ba]
    (let [ba-len   (alength ba)
          uint-len (write-dynamic-uint out ba-len)]
      (.write out ba 0 ba-len)
      (+ ba-len uint-len))

    (write-dynamic-uint out 0)))

(defn read-ba ^bytes [^DataInput in len] (let [ba (byte-array len)] (.readFully in ba) ba))
(defn read-?ba       [^DataInput in len] (when (pos? ^long len) (read-ba in len)))

(defn skip-dynamic-ba        [^DataInput in] (.skipBytes in (read-dynamic-uint in)))
(defn read-dynamic-ba ^bytes [^DataInput in] (read-ba    in (read-dynamic-uint in)))
(defn read-dynamic-?ba       [^DataInput in] (read-?ba   in (read-dynamic-uint in)))
(defn read-dynamic-?ba*      [^DataInputStream in]
  (let [n0 (.available in)]
    [(read-dynamic-?ba in) (- n0 (.available in))]))

(defn write-dynamic-str   ^long [^DataOutput out ?s] (write-dynamic-ba out (?str->?utf8-ba ?s)))
(defn  read-dynamic-str ^String [^DataInput  in]     (utf8-ba->str   (read-dynamic-ba  in)))
(defn  read-dynamic-?str        [^DataInput  in]     (?utf8-ba->?str (read-dynamic-?ba in)))

;;;; `BitSet` utils

(defn bitset->ba
  "Returns byte[] containing the bits in given `java.util.BitSet`."
  {:added "v3.72.0 (2023-10-24)"}
  ^bytes [^BitSet bs] (.toByteArray bs))

(defn ba->bitset
  "Returns new `java.util.BitSet` containing the bits in given byte[]."
  ;; Strange that Java doesn't have a built-in for this
  {:added "v3.72.0 (2023-10-24)"}
  ^BitSet [^bytes ba]
  (let [bs (BitSet. (* (alength ba) 8))]
    (areduce ba byte-idx _ bs
      (dotimes [bit-idx 8]
        (when (bit-test (aget ba byte-idx) bit-idx)
          (.set bs (+ (* byte-idx 8) bit-idx)))))
    bs))

(comment (ba->bitset (bitset->ba (doto (BitSet.) (.set 0) (.set 3) (.set 5)))))

(defn reduce-bitset
  "Reduces given `java.util.BitSet`, calling (rf <acc> <bit-idx>) for the int
  index of each bit in BitSet."
  {:added "v3.72.0 (2023-10-24)"}
  [rf init ^BitSet bs]
  (loop [bit-idx (.nextSetBit bs 0)
         acc     init]
    (if (>= bit-idx 0)
      (let [result (rf acc bit-idx)]
        (if (reduced? result)
          (do                                  @result)
          (recur (.nextSetBit bs (inc bit-idx)) result)))
      acc)))

(comment (reduce-bitset conj #{} (doto (BitSet.) (.set 0) (.set 3) (.set 5))))

(defn freeze-set
  "Given a coll of elements `els` and {<element> <unique-bit-idx>} bit schema,
  returns a minimal serialized byte[] that can be deserialized with `thaw-set`:
    (thaw-set {0 :el0 1 :el1 2 :el2}
      (freeze-set {:el0 0 :el1 1 :el2 2} #{:el0 :el2})) => #{:el0 :el2}"
  {:added "v3.72.0 (2023-10-24)"}
  [{:keys [freeze/skip-unknown?] :as bit-schema} els]
  (when-not (or (empty? els) (empty? bit-schema))
    (let [bs (java.util.BitSet.)]
      (run!
        (fn [el]
          (if-let [bit-idx (get bit-schema el)]
            (.set bs (int bit-idx))
            (if skip-unknown?
              nil
              (throw
                (ex-info "Failed to freeze set (encountered element not in bit schema)"
                  {:element    {:type (type el) :value el}
                   :bit-schema bit-schema})))))
        els)
      (.toByteArray bs))))

(comment (vec (freeze-set {:el0 0 :el1 1 :el2 2} [:el0 :el1 :el1])))

(defn thaw-set
  "Given serialized byte[] output from `freeze-set` and {<bit-idx> <element>}
  bit schema, returns the set of elements encoded in byte[]:
    (thaw-set {0 :el0 1 :el1 2 :el2}
      (freeze-set {:el0 0 :el1 1 :el2 2} #{:el0 :el2})) => #{:el0 :el2}"
  {:added "v3.72.0 (2023-10-24)"}
  [{:keys [thaw/skip-unknown?] :as bit-schema} ?ba]
  (when-let [^bytes ba ?ba]
    (when-not (empty? bit-schema)
      (let [bs (ba->bitset ba)]
        (persistent!
          (reduce-bitset
            (fn [els bit-idx]
              (if-let [el (get bit-schema bit-idx)]
                (conj! els el)
                (if skip-unknown?
                  els
                  (throw
                    (ex-info "Failed to thaw set (encountered index not in bit schema)"
                      {:bit-index  bit-idx
                       :bit-schema bit-schema})))))
            (transient #{}) bs))))))

(comment
  (thaw-set {0 :el0 1 :el1 2 :el2}
    (freeze-set {:el0 0 :el1 1 :el2 2} #{:el0 :el2})))
