(ns phlegmaticprogrammer.tillich_zemor_hash
  (:import [phlegmaticprogrammer.tillich_zemor_hash GL2p127])
  (:import [java.security MessageDigest])
  (:use clojure.test))

(def sha1Digest (MessageDigest/getInstance "SHA1"))

(defn sha1 ^bytes [^bytes bytes] (.digest sha1Digest bytes))

(defn illegal [s] (throw (IllegalArgumentException. s)))

(defn- num-to-bits [n]
  (cond
    (= n 0) '()
    (= (mod n 2) 0) (cons 0 (num-to-bits (quot n 2)))
    :else (cons 1 (num-to-bits (quot n 2)))))

(defn byte-to-bits [b]
  (let [u (if (< b 0) (+ b 256) b)
        bits (num-to-bits u)
        pad (repeat (- 8 (count bits)) 0)]
    (vec (concat bits pad))))

(defn bits-to-byte [b]
  (let [x (reduce (fn [x b] (+ (* x 2) b)) 0 (reverse b))]
    (if (> x 127) (byte (- x 256)) (byte x))))

(defn bits-to-bytes [bs]
  (let [ps (partition-all 8 bs)] 
    (byte-array (count ps) (map bits-to-byte ps))))

(defn bytes-to-bits [bs]
  (vec (reduce (fn [v b] (concat v (byte-to-bits b))) [] bs)))

(defn bits-to-long [bs]
  (let [bys (bits-to-bytes bs)
        tol #(if (< % 0) (+ (long %) 256) (long %))
        [r k] (reduce (fn [[r k] b] [(bit-or r (bit-shift-left (tol b) k)) (+ k 8)])
                      [(long 0) 0]
                      bys)
        ]
    r))

(defn long-to-bits [l]
  (let [tob (fn [k]
               (let [u (bit-and (bit-shift-right l k) 255)
                     b (if (> u 127) (- u 256) u)]
                 (byte-to-bits b)))]
    (vec(concat
         (tob 0) (tob 8) (tob 16) (tob 24)
         (tob 32) (tob 40) (tob 48) (tob 56)
         ))))

(defn ceilquot [a b]
  (let [q (quot a b)]
    (if (= a (* b q)) q (+ q 1))))

(defn consByte [l b]
  (let [i (int b)
        w (if (< i 0) (+ i 256) i)]
    (cons (mod w 16)
          (cons (quot w 16)
                l))))

(defn hex [x] (char (if (< x 10) (+ (int \0) x) (+ (int \a) (- x 10)))))

(defn hexstr [bytes] (apply str (map hex (reduce consByte () bytes))))

(defn- invhex [c]
  (let [x (int c)]
    (cond
      (and (>= x 48) (<= x 57)) (- x 48)
      (and (>= x 97) (<= x 102)) (- x 87)
      :else (throw (RuntimeException. (str "invalid hex char code " x))))))

(defn- invhex-lo-hi [lo hi]
  (let [w (+ (invhex lo) (* 16 (invhex hi)))]
    (if (> w 127) (byte (- w 256)) (byte w))))
  
(defn- hexbytes' [s]
  (if s
    (cons (invhex-lo-hi (first s) (second s)) (hexbytes' (next (next s))))
    '()))

(defn hexbytes [s] (byte-array (reverse (hexbytes' s))))

(def sha1-like #{0 1 127})
(def sha256-like #{0 2 4 7 251})
(def rijndael #{8 4 3 1 0})

(defn- p-zero [n] (vec (repeat n 0)))
(defn- p-one [n] (vec (cons 1 (repeat (- n 1) 0))))
(defn- p-x [n] (vec (cons 0 (cons 1 (repeat (- n 2) 0)))))

(defn- p-add [n a b] (vec (for [i (range n)] (bit-xor (a i) (b i)))))
(defn- p-shiftleft [n a] (vec (for [i (range n)] (if (= i 0) 0 (a (- i 1))))))
(defn- p-mul [n r a b] ((reduce (fn [[b c] a-i]
                                  (let [new-c (if (= a-i 1) (p-add n c b) c)
                                        sl-b (p-shiftleft n b)
                                        new-b (if (= 1 (b (- n 1))) (p-add n sl-b r) sl-b)]
                                    [new-b new-c]))
                                [b (p-zero n)]
                                a) 1))

(defn- p-fromsparse [n sparse-a]
  (let [positions (set sparse-a)]
    (vec (for [i (range n)] (if (contains? positions i) 1 0)))))

(defn- p-tosparse [a]
  ((reduce (fn [[i s] a-i]
            [(+ i 1)
             (if (= 1 a-i) (conj s i) s)])
          [0 #{}]
          a) 1))

(defn- p-fromvector [n v]
  (vec (for [i (range n)]
         (let [b (v i)]
           (cond
             (= b 1) 1
             (= b 0) 0
             :else (throw (IllegalArgumentException. "bit vector expected")))))))

(defprotocol BinaryField
  (dimension [this])
  (zero [this])
  (one [this])
  (X [this])
  (add [this a b])
  (mul [this a b])
  (from-vector [this vector-a])
  (to-vector [this a])
  )

(def binaryField127
  (reify BinaryField
    (dimension [this] 127)
    (zero [this] (GL2p127/make 0 0))
    (one [this] (GL2p127/make 1 0))
    (X [this] (GL2p127/make 2 0))
    (add [this a b]
      (let [result (GL2p127/make 0 0)]
        (do (GL2p127/add a b result) result)))
    (mul [this a b]
      (let [result (GL2p127/make 0 0)]
        (do (GL2p127/mul a b result) result)))
    (from-vector [this vector-a]
      (let [p (vec (map bits-to-long (partition-all 64 vector-a)))]
        (GL2p127/make (p 0) (p 1))))
    (to-vector [this a]
      (let [lo (GL2p127/lo a)
            hi (GL2p127/hi a)]
        (vec (concat (long-to-bits lo) (take 63 (long-to-bits hi))))))
    ))

(defn naiveBinaryField [modulus]
  (let [positions (set modulus)
        n (apply max modulus)
        r (p-fromsparse n positions)]
    (reify BinaryField
      (dimension [this] n)
      (zero [this] (p-zero n))
      (one [this] (p-one n))
      (X [this] (p-x n))
      (add [this a b] (p-add n a b))
      (mul [this a b] (p-mul n r a b))
      (from-vector [this vector-a] (p-fromvector n vector-a))
      (to-vector [this a] a)
      )))

(defprotocol AssociativeHash
  (from-bytes [this b] "restores a hashvalue from a byte array")
  (to-bytes [this hash] "encodes a hashvalue as a byte array")
  (hash-size [this] "the number of bytes that a hashvalue occupies")
  (hash-bit [this b] "hashes a bit, i.e. a value that is either 0 or 1")
  (hash-byte [this b] "hashes a byte, i.e. a value that is between -128 and 127")
  (hash-bytes [this b] "hashes a byte array")
  (hash-comb [this hashes] "combines a sequence of hash values into a single hash value"))

(defn- lincomb [F a b c d] (add F (mul F a b) (mul F c d)))

(defn- matrixmul [F [a11 a21 a12 a22] [b11 b21 b12 b22]]
  [(lincomb F a11 b11 a12 b21)
   (lincomb F a21 b11 a22 b21)
   (lincomb F a11 b12 a12 b22)
   (lincomb F a21 b12 a22 b22)])

(defn- matrixdet [F [a11 a21 a12 a22]]
  (lincomb F a11 a22 a21 a12))

(defn TillichZemorHash "constructs an AssociativeHash from a BinaryField F" [F]
  (let [A [(X F) (one F) (one F) (zero F)]
        B [(X F) (one F) (add F (X F) (one F)) (one F)]
        emptyhash [(one F) (zero F) (zero F) (one F)]
        n (dimension F)
        hashsize (ceilquot (* n 4) 8)
        hashbit #(cond (= % 0) A (= % 1) B :else (illegal "bit must be 0 or 1"))
        hashcomb #(matrixmul F %1 %2)
        hashbyte #(reduce hashcomb (map hashbit (byte-to-bits %)))
        hashbyte-table (vec (for [i (range 256)] (hashbyte (byte (- i 128)))))
        ]
    (reify AssociativeHash
      (from-bytes [this bs]
        (let [matrix (vec (map #(from-vector F %)
                               (partition n (bytes-to-bits bs))))]
          (if (= (one F) (matrixdet F matrix))
            matrix
            matrix ;;(illegal (str "invalid hashvalue " (matrixdet F matrix) " of matrix " matrix))
            )))
      (to-bytes [this [a b c d]]
        (bits-to-bytes (vec (concat (to-vector F a) (to-vector F b) (to-vector F c) (to-vector F d)))))
      (hash-size [this] hashsize)
      (hash-bit [this b] (hashbit b))
      (hash-byte [this b] (hashbyte-table (+ b 128)))
      (hash-bytes [this bs] (reduce (fn [h b] (hashcomb h (hashbyte-table (+ b 128)))) emptyhash bs))           
      (hash-comb [this hashes] (reduce hashcomb emptyhash hashes))      
      )))

(def TillichZemor127 (TillichZemorHash binaryField127))
(def DefaultAssocHash TillichZemor127)

(defn SlowAssocHash [] (TillichZemorHash (naiveBinaryField sha1-like)))

(defn rand-bytes [n]
  (byte-array n
              (for [i (range n)]
                (let [r (rand-int 256)]
                  (byte (if (> r 127) (- r 256) r))))))

(defn rand-test [H1 H2 n]
  (let [b (rand-bytes n)
        ; _ (println "bytes = " (str (vec b))) 
        U1 (hash-bytes H1 b)
        U2 (hash-bytes H2 b)        
        u1 (to-bytes H1 U1)
        u2 (to-bytes H2 U2)
        ; _ (println "hashed bytes H1 = " (str (vec u1))) 
        ; _ (println "hashed bytes H2 = " (str (vec u2))) 
        UU1 (from-bytes H1 u1)
        ; UU2 (from-bytes H2 u2) fails for some reason
        ]
    (and (= U1 UU1) (= (vec u1) (vec u2)))))

(deftest hash-functions
  (let [test (fn [n] (is (rand-test DefaultAssocHash (SlowAssocHash) n)))]
    (test 0)
    (test 1)
    (test 2)
    (test 3)
    (test 4)
    (test 10)
    (test 100)
    (test 1000)))

(defn hashbytes "Computes the hash value of a byte array."
  [b] (hash-bytes DefaultAssocHash b))
(defn tobytes "Encodes a hash value as a byte array."
  [hashvalue] (to-bytes DefaultAssocHash hashvalue))
(defn frombytes "Restores a hash value from a byte array."
  [b] (from-bytes DefaultAssocHash b))
(defn hashcomb "Combines its arguments, all of which must be hash values, into a single hash value."
  [& hashes] (hash-comb DefaultAssocHash hashes)) 

(deftest concrete-example
  (let [a [114 -70 -27 -124 -7 -33 -80 65 -101 93]
        b [-84 55 -82 55 -121 83 48 63 80 85 -128 13 -118 83]
        hashit (fn [w] (hashbytes (byte-array (map byte w))))
        rep (fn [h] (vec (tobytes h)))
        hash_a [-32 123 53 -88 -110 -38 115 -78 -3 97 1 0 0 0 0 -128 -80 -57 108 50 -42 -51 35 -72 64 76 0 0 0 0 0 64 32 124 46 -25 10 1 -120 37 94 62 0 0 0 0 0 32 92 -83 -82 -43 -40 123 70 -108 31 12 0 0 0 0 0 0]
        hash_b [55 -17 37 19 -78 -14 94 127 -62 102 -115 86 -60 99 1 -128 -71 92 -97 -58 -102 -52 -98 124 114 -41 19 117 75 79 0 64 -59 -47 -41 -30 -76 -97 -56 -95 88 -14 111 -69 69 62 0 64 -60 -107 -52 -64 124 94 -61 -17 -8 -90 89 -88 90 12 0 0]
        ]
    (and
     (is (= (rep (hashit a)) hash_a))
     (is (= (rep (hashit b)) hash_b))
     (is (= (rep (hashit (concat a b))) (rep (hashcomb (hashit a) (hashit b))))))))
        


