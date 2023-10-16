(ns ^:no-doc taoensso.encore.bytes
  "Experimental, subject to change without notice!!
  Private low-level byte[] utils."
  {:added "vX.Y.Z (YYYY-MM-DD)"}
  (:refer-clojure :exclude [bytes?])
  (:require
   [taoensso.encore :as enc :refer [have have?]])

  (:import
   [java.nio.charset StandardCharsets]
   [java.io
    DataInput  DataInputStream
    DataOutput DataOutputStream
    ByteArrayInputStream
    ByteArrayOutputStream]))

(comment
  (remove-ns 'taoensso.encore.bytes)
  (:api (enc/interns-overview)))

;;;; Aliases

(enc/defaliases
  enc/bytes?
  enc/ba=
  enc/str->utf8-ba
  enc/utf8-ba->str)

;;;; Basics

(def ^:private ^:const utf8-str "hello ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸ")
(defn ba-len ^long [?ba] (if ?ba (alength ^bytes ?ba) 0))

(defn ba-join*
  "Returns byte[] concatenation of >= 0 ?byte[]s."
  ^bytes [bas]
  (let [total-len (reduce (fn [^long acc in] (if in (+ acc (alength ^bytes in)) acc)) 0 bas)
        out       (byte-array total-len)]
    (loop [idx 0, remaining bas]
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
    (^bytes [       ]        ba0)
    (^bytes [ba     ] (or ba ba0))
    (^bytes [ba1 ba2]
     (enc/cond
       (nil? ba1) (or ba2 ba0)
       (nil? ba2) (or ba1 ba0)
       :else
       (let [l1  (alength ^bytes ba1)
             l2  (alength ^bytes ba2)
             out (byte-array (+ l1 l2))]
         (System/arraycopy ba1 0 out 0  l1)
         (System/arraycopy ba2 0 out l1 l2)
         (do                     out))))

    (^bytes [ba1 ba2 & more]
     (ba-join (ba-join ba1 ba2) (ba-join* more)))))

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

;;;; To/from integers

(do
  (def ^:const ubyte-max  "Max unsigned byte: 255"          (-    Byte/MAX_VALUE    Byte/MIN_VALUE))
  (def ^:const ushort-max "Max unsigned short: 65,535"      (-   Short/MAX_VALUE   Short/MIN_VALUE))
  (def ^:const uint-max   "Max unsigned int: 4,294,967,295" (- Integer/MAX_VALUE Integer/MIN_VALUE))

  (let [fail!
        (fn [n target-type]
          (throw
            (ex-info "Numerical overflow"
              {:target-type target-type
               :given {:value n :type (type n)}})))]

    (defn as-byte   ^long [^long n] (if (and (>= n    Byte/MIN_VALUE) (<= n    Byte/MAX_VALUE)) n (fail! n :byte)))
    (defn as-short  ^long [^long n] (if (and (>= n   Short/MIN_VALUE) (<= n   Short/MAX_VALUE)) n (fail! n :short)))
    (defn as-int    ^long [^long n] (if (and (>= n Integer/MIN_VALUE) (<= n Integer/MAX_VALUE)) n (fail! n :int)))

    (defn as-ubyte  ^long [^long n] (if (and (>= n 0) (<= n ubyte-max))  n (fail! n :ubyte)))
    (defn as-ushort ^long [^long n] (if (and (>= n 0) (<= n ushort-max)) n (fail! n :ushort)))
    (defn as-uint   ^long [^long n] (if (and (>= n 0) (<= n uint-max))   n (fail! n :uint)))))

(do
  (defn to-ubyte        "ℤ[-128,127] -> ℕ[0,255]"   ^long [^long n] (as-ubyte  (- n    Byte/MIN_VALUE)))
  (defn to-ushort   "ℤ[-32768 32767] -> ℕ[0,65535]" ^long [^long n] (as-ushort (- n   Short/MIN_VALUE)))
  (defn to-uint                   "ℤ -> ℕ[0,max]"   ^long [^long n] (as-uint   (- n Integer/MIN_VALUE)))

  (defn from-ubyte    "ℕ[0,255] -> ℤ[-128,127]"     ^long [^long n] (+ (as-ubyte  n)    Byte/MIN_VALUE))
  (defn from-ushort "ℕ[0,65535] -> ℤ[-32768,32767]" ^long [^long n] (+ (as-ushort n)   Short/MIN_VALUE))
  (defn from-uint     "ℕ[0,max] -> ℤ"               ^long [^long n] (+ (as-uint   n) Integer/MIN_VALUE))

  (defn n-bytes->n-bits ^long [n-bytes] (*    (int n-bytes) 8))
  (defn n-bits->n-bytes ^long [n-bits]  (quot (int n-bits)  8)))

(do
  (defn  byte->ba ^bytes [n] (let [bb (java.nio.ByteBuffer/allocate             1)] (.put      bb (byte  (as-byte  n))) (.array bb)))
  (defn short->ba ^bytes [n] (let [bb (java.nio.ByteBuffer/allocate   Short/BYTES)] (.putShort bb (short (as-short n))) (.array bb)))
  (defn   int->ba ^bytes [n] (let [bb (java.nio.ByteBuffer/allocate Integer/BYTES)] (.putInt   bb (int   (as-int   n))) (.array bb)))

  (defn ba->byte  [^bytes ba] (aget ba 0))
  (defn ba->short [^bytes ba] (let [bb (java.nio.ByteBuffer/allocate   Short/BYTES)] (.put bb ba) (.flip bb) (.getShort bb)))
  (defn ba->int   [^bytes ba] (let [bb (java.nio.ByteBuffer/allocate Integer/BYTES)] (.put bb ba) (.flip bb) (.getInt   bb))))

(comment
  (enc/qb 1e6 ; [66.26 68.72 70.05]
    (ba->byte  (byte->ba  25))
    (ba->short (short->ba 25))
    (ba->int   (int->ba   25))))

;;;; To/from strings
;; Java strings are UTF-16, but we'll use UTF-8 encoding when converting to/from bytes

(def ^:const utf8-str "ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸ")
(defn utf8-?ba->str [?ba] (when-let [ba ?ba] (enc/utf8-ba->str ba)))
(defn ?str->utf8-ba [?s]  (when-let [s  ?s]  (enc/str->utf8-ba  s)))
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
  ([target-len x] (when x (as-ba target-len x)))
  ([           x] (when x (as-ba            x))))

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

(defn parse-buffer-len [spec]
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

(defn read-ba! ^bytes [^DataInput in len] (let [ba (byte-array len)] (.readFully in ba) ba))
(defn read-ba         [^DataInput in len] (when (pos? ^long len) (read-ba! in len)))

(defn write-dynamic-uint
  "Writes given unsigned int to `out` as 1-5 bytes.
  Returns the number of bytes written."
  ^long [^DataOutput out unsigned-int]
  (let [n (long unsigned-int)]
    Byte/MIN_VALUE
    ;; [-128,124] used for [0,252] ; (from-ubyte 252)
    ;; [125, 127] used to indicate typed size prefix
    (enc/cond
      (<= n        252) (do                      (.writeByte  out (from-ubyte  n)) 1) ; 1     byte  for [0,252]
      (<= n  ubyte-max) (do (.writeByte out 125) (.writeByte  out (from-ubyte  n)) 2) ; 1+1=2 bytes for [0,255]
      (<= n ushort-max) (do (.writeByte out 126) (.writeShort out (from-ushort n)) 3) ; 1+2=3 bytes for [0,65535]
      (<= n   uint-max) (do (.writeByte out 127) (.writeInt   out (from-uint   n)) 5) ; 1+4=5 bytes for [0,4294967295]
      :else
      (throw
        (ex-info "Dynamic unsigned integer exceeds max"
          {:value n, :max uint-max})))))

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

(defn write-dynamic-str
  ^long [^DataOutput out ?s] (write-dynamic-ba out (?str->utf8-ba ?s)))

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

(defn skip-dynamic-ba         [^DataInput in] (.skipBytes in (read-dynamic-uint in)))
(defn read-dynamic-ba         [^DataInput in] (read-ba    in (read-dynamic-uint in)))
(defn read-dynamic-ba! ^bytes [^DataInput in] (read-ba!   in (read-dynamic-uint in)))
(defn read-dynamic-ba*        [^DataInputStream in]
  (let [n0 (.available in)]
    [(read-dynamic-ba in) (- n0 (.available in))]))

(defn read-dynamic-str          [^DataInput in]     (utf8-?ba->str (read-dynamic-ba  in)))
(defn read-dynamic-str! ^String [^DataInput in] (enc/utf8-ba->str  (read-dynamic-ba! in)))

(do
  (defn write-ubyte  "Writes 1 byte"  [^DataOutput out n] (.writeByte  out (from-ubyte  n)))
  (defn write-ushort "Writes 2 bytes" [^DataOutput out n] (.writeShort out (from-ushort n)))
  (defn write-uint   "Writes 4 bytes" [^DataOutput out n] (.writeInt   out (from-uint   n)))

  (defn read-ubyte  "Reads 1 byte"  ^long [^DataInput in] (to-ubyte  (.readByte  in)))
  (defn read-ushort "Reads 2 bytes" ^long [^DataInput in] (to-ushort (.readShort in)))
  (defn read-uint   "Reads 4 bytes" ^long [^DataInput in] (to-uint   (.readInt   in))))
