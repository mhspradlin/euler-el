;; P1
;; Sum all multiples of 3 or 5 less than 1000
(setq euler1 0)
(dotimes (i 1000)
  (if (or (= (mod i 3) 0)
          (= (mod i 5) 0))
      (setq euler1 (+ euler1 i))))
euler1

(defmacro comment (&rest sexp)
  "Ignores all forms inside"
  nil)

(comment
 euler1)

(require 'generator)
(setq lexical-binding t) ; Required for iterators to work

;; P2
;; Find sum of even-valued fibonnaci numbers less than four million
(iter-defun fibonacci ()
  "An infinite fibonacci sequence"
  (setq prev 0
        curr 1)
  (iter-yield prev)
  (iter-yield curr)
  (while t
    (setq next (+ prev curr)
          prev curr
          curr next)
    (iter-yield next)))

(iter-defun ifilter (filterp generator)
  (iter-do (i generator)
    (if (funcall filterp i)
        (iter-yield i))))
(iter-defun itake-while (whilep generator)
  (iter-do (i generator)
    (if (funcall whilep i)
        (iter-yield i)
        (signal 'iter-end-of-sequence nil))))
(defun ireduce (reducer initial-accum generator)
  (let ((accum initial-accum))
    (iter-do (i generator)
      (setq accum (funcall reducer accum i)))
    accum))

(comment
 (iter-defun test-gen () (iter-yield 1) (iter-yield 2))
 (ireduce #'(function +) 0 (test-gen)) ; void-function reducer
 (ireduce (lambda (x y) (+ x y)) 0 (test-gen)) ; void-function reducer
 (ireduce (function (lambda (x y) (+ x y))) 0 (test-gen)) ; void-function reducer
 (ireduce #'(lambda (x y) (+ x y)) 0 (test-gen)) ; void-function reducer
 (ireduce #'+ 0 (test-gen))
 (ireduce (function +) 0 (test-gen))
 (iter-next (itake-while #'(lambda (x) (< x 2)) (test-gen)))
 (defun some-fibs ()
   (let ((fibs '()))
     (iter-do (i (itake-while #'(lambda (x) (< x 100)) (fibonacci)))
       (push i fibs))
     fibs))
 (some-fibs)
  (ireduce #'+ 0
          (itake-while #'(lambda (x) (< x 4000000))
                       (ifilter #'(lambda (x) (= (mod x 2) 0))
                                (fibonacci))))
 )

;; P3
; What is the largest prime factor of 600851475143

(require 'dash)
(iter-defun naturals ()
  "All natural numbers (0, 1, 2, ...)"
      (let ((n 0))
    (while t
      (iter-yield n)
      (setq n (+ n 1)))))
(defun divides? (numerator denominator)
  (= 0 (mod numerator denominator)))
(defun divisible-by-anyp (divisors n)
  "True if any element of list divisors evenly divides n"
  (-any? (-partial #'divides? n) divisors))
(iter-defun primes ()
  (let ((found-primes (make-queue))
        (natural-numbers (naturals)))
    (iter-next natural-numbers) ; Skip 0
    (iter-next natural-numbers) ; Skip 1
    (iter-do (i natural-numbers)
      (if (not (divisible-by-anyp (queue-all found-primes) i))
          (progn (iter-yield i)
                 (queue-enqueue found-primes i))))))

(defun itake-n (n generator)
  (let ((vals '()))
    (iter-do (i (itake-while #'(lambda (x) (length< vals n)) generator))
      (!cons i vals))
    vals))

(require 'queue)

(defun largest-prime-factor (n)
  (let* ((stop (floor (sqrt n)))
         (possible-factors (itake-while #'(lambda (x) (<= x stop)) (primes)))
         (factors (ifilter #'(lambda (x) (divides? n x)) possible-factors))
         (factor-list '(1)))
    (iter-do (i possible-factors)
      (push i factor-list))
    (apply #'max factor-list)))

(defun largest-prime-factor-2 (n)
  (let* ((max-possible-factor (floor (sqrt n)))
         (two-to-max-factor (-iterate #'1+ 2 max-possible-factor))
         (factors '())
         (prime-factors '()))
    (dolist (i two-to-max-factor)
      (if (divides? n i)
          (push i factors)))
    (dolist (i factors)
      (if (not (divisible-by-anyp (--remove-first (= it i) factors) i))
          (push i prime-factors)))
    (apply #'max prime-factors)))

(comment
 (iter-next (test-gen))
 (iter-next (naturals))
 (iter-next (itake-while #'(lambda (x) (< x 1000)) (naturals)))
 (iter-next (primes))
 (itake-n 10 (naturals))
 (itake-n 10 (primes))
 (itake-n 100 (ifilter #'(lam)
                       (itake-while #'(lambda (x) (< x 435))
                                    (primes))))
 (let ((l '(2)))
   (!cons 3 l))
 (divides? 15 3)
 (divisible-by-anyp '(2 3 5) 15)
 (largest-prime-factor 15)
 (largest-prime-factor 2323)
 (gcmh-time (largest-prime-factor 23232323)) ; 1.3s, scaling is at least linear so...
 (largest-prime-factor 2323232323)
 (largest-prime-factor 600851475143) ; Too slow, probably due to checking factors in opposite order
 (gcmh-time (itake-n 100 (primes))) ; 0.5s
 (gcmh-time (itake-n 1000 (primes))) ; 1.6s
 (gcmh-time (itake-n 2000 (primes))) ; 6.8s
 (gcmh-time (itake-n 4000 (primes))) ; 29.6s -- seems to be O(n^2) ish
                                     ; Need a data structure with push-back, like queue
                                     ; Tried queue from queue.el, didn't work
                                     ; Should profile, or find another approach
                                     ;
 (largest-prime-factor-2 600851475143) ; Takes like 2s, works great => 6857
 )

;; P4
;; Find largest palindrome that is product of two 3-digit numbers

(defun is-palindrome? (n)
  (let ((n-string (number-to-string n)))
    (equal n-string (reverse n-string))))

(defun euler-p4 ()
  (let* ((three-digit-nums (-iterate #'1+ 100 900))
         (three-digit-sums '())
         (palindromes '()))
    (dolist (i three-digit-nums)
      (dolist (j three-digit-nums)
        (push (* i j) three-digit-sums)))
    (setq palindromes (-filter #'is-palindrome? three-digit-sums))
    (apply #'max palindromes)))

(defun euler-p4-arrow ()
  (--> (-iterate #'1+ 100 900)
       (-table-flat #'list it it)
       (-map #'-product it)
       (-filter #'is-palindrome? it)
       (apply #'max it)))

(comment
 (is-palindrome? 100)
 (is-palindrome? 101)
 (-iterate #'1+ 100 900)
 (euler-p4) ; => 906609
 (euler-p4-arrow) ; Same answer
 (-table-flat #'list '(1 2 3) '(1 2 3))
)

;; p5
;; What is the smallest positive number evenly divisible by all numbers from 1 to 10?

;; Approach: There's some overlap in the numbers -- if 2 and 4 are factors, then
;; 8 is already covered so we don't need to add it in.
;; So, let's start from the highest number. Calculate its factors, then only
;; add lower numbers if they are not a factor of an already added number.
;; I don't think that will work. Consider 20, then 16. 16 is not a factor of 20, but we
;; would add 4 twos on top of the 2 twos we already have from 20. Not so good.
;; Let's try to check all the numbers first and go from there if it's too slow.

(defun int-range (start-inclusive end-exclusive)
  "Create a list from start-inclusive to end-exclusive"
  (-iterate #'1+ start-inclusive (- end-exclusive start-inclusive)))

(defun divisible-by-all? (divisors n)
  "Returns t if n is divisible by all of divisors"
  (-all? (-partial #'divides? n) divisors))

;; This takes too long checking all of 1 to 20
;; Remove obvious ones:
;; - No need to check 1
;; - If divisible by 10 and 2, then also divisible by 20
;; - Vice versa more useful, if divisible by 20 then also by 4, 2, 10, etc.
;; - (11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
;; Still too long
;; No sense in moving by ones, since result will be even
;; Can we move by twenties? I think so, since we know it's a multiple of 20
;; Moving by twenties, no need to check for divisibility by 20 or its divisors
;; It takes a minute or so, but works
(defun euler-p5 ()
  (let* ((eleven-to-nineteen (int-range 11 20))
         (current-num 20)
         (found nil))
    (while (not found)
      (if (divisible-by-all? eleven-to-nineteen current-num)
          (setq found t)
          (setq current-num (+ 20 current-num))))
    current-num))

(comment
 (int-range 10 20)
 (divisible-by-all? '(1 2 3) 6)
 (divisible-by-all? '(1 2 3) 7)
 (euler-p5) ; Works, takes a minute => 232792560
 )

;; P6
;; Sum of squares of 1 -> 100 minus square of sum of 1 -> 100

(defun euler-p6 ()
  (let* ((one-to-one-hundred (number-sequence 1 100))
         (sum-of-squares (--> one-to-one-hundred
                              (seq-map #'(lambda (n) (expt n 2)) it)
                              (seq-reduce #'+ it 0)))
         (square-of-sums (--> one-to-one-hundred
                              (seq-reduce #'+ it 0)
                              (expt it 2))))
    (- sum-of-squares square-of-sums)))

(comment
 (euler-p6))

;; P7
;; What is the 10001st prime?

;; I'll try a prime sieve this time with a vector and avoiding function calls

;; The seq is assumed to have nil values only at the end of it, at which point we stop
(defun divisible-by-none? (divisors-seq n)
  (let ((index 0)
        (len (length divisors-seq))
        (divisible-by-none t))
    (while (< index len)
      (if-let ((divisor (elt divisors-seq index)))
          (if (= 0 (mod n divisor))
            (progn
              (setq divisible-by-none nil)
              (setq index len))
            (setq index (1+ index)))
          (setq index len)))
    divisible-by-none))


(defun nth-prime (n)
  (let* ((primes (make-vector n nil))
         (found-primes-count 1)
         (current-num 3))
    (aset primes 0 2)
    (while (< found-primes-count n)
      (if (divisible-by-none? primes current-num)
          (progn
            (aset primes found-primes-count current-num)
            (setq found-primes-count (1+ found-primes-count))))
      (setq current-num (1+ current-num)))
    (elt primes (1- n))))

(comment
 (nth-prime 2)
 (nth-prime 100)
 (nth-prime 1000)
 (nth-prime 10001) ; => 104743 works, but takes a minute or so
 (divisible-by-none? (vector 2 3 nil nil) 6)
 (divisible-by-none? (vector 2 3 nil nil) 7)
 (divisible-by-none? (vector 2 3) 7)
 )

;; P8
;; Find thirteen adjacent digits in 1000-digit number that has the greatest product

(setq p8-number 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)

(defun euler-p8 ()
  (--> (number-to-string p8-number)
       (string-to-list it)
       (-partition-in-steps 13 1 it)
       (seq-map (lambda (partition)
                  (-as-> partition x
                         (seq-map #'char-to-string x)
                         (seq-map #'string-to-number x)
                         (seq-reduce #'* x 1)))
                it)
       (apply #'max it)))

(comment
 (euler-p8) ; Correctly => 23514624000
 )

;; P9
;; Find the Pythagorean Triplet (a < b < c, a^2 + b^2 = c^2) where a + b + c = 1000
;; then multiply a * b * c

;; This works fine, but it's too slow to generate ALL the combos then filter
;; down to the ones where the sum is 1000
(iter-defun triplet-abcs ()
  (let ((a 1)
        (b 2)
        (c 3))
    (while t
      (iter-yield (list a b c))
      (cond ((and (= a (1- b)) (= b (1- c)))
                (setq a 1
                      b 2
                      c (1+ c)))
            ((= a (1- b))
                (setq a 1
                      b (1+ b)))
            (t (setq a (1+ a)))))))

(iter-defun triplet-abcs-sum-to (n)
  (let ((a 1)
        (b 2)
        (c (- n 3)))
    (ignore-errors
      (while t
        (iter-yield (list a b c))
        (cond )))))

(defun euler-p9 ()
  (--> (triplet-abcs)
       (ifilter (lambda (abc) (= 10 (apply #'+ abc))) it)))

(defun euler-p9-2 ()
   (let ((a 1)
        (b 2)
        (c 3))
    (while t
      (if (and (= 1000 (+ a b c)) (= (+ (expt a 2) (expt b 2)) (expt c 2)))
          (signal 'found (list a b c)))
      (cond ((and (= a (1- b)) (= b (1- c)))
                (setq a 1
                      b 2
                      c (1+ c)))
            ((= a (1- b))
                (setq a 1
                      b (1+ b)))
            (t (setq a (1+ a)))))))

(comment
 (itake-n 10 (triplet-abcs))
 (itake-n 1 (euler-p9))
 (euler-p9-2) ; 200 375 425
 (* 200 375 425) ; 31875000 which is correct
 )

;; P10
;; Sum of all primes below two million

(comment
 (nth-prime 15000)
 (byte-compile 'nth-prime)
 (nth-prime 15000) ; 163841 seems faster after byte-compiling
 (gcmh-time (nth-prime 5000)) ; 6.273944535
 (byte-compile 'divisible-by-none?)
 (gcmh-time (nth-prime 5000)) ; 0.455887839 => Byte-compiling all functions involved is best
 )


(byte-compile 'divisible-by-none?)

(defun n-primes (n)
  (let* ((primes (make-vector n nil))
         (found-primes-count 1)
         (current-num 3))
    (aset primes 0 2)
    (while (< found-primes-count n)
      (if (divisible-by-none? primes current-num)
          (progn
            (aset primes found-primes-count current-num)
            (setq found-primes-count (1+ found-primes-count))))
      (setq current-num (+ 2 current-num)))
    primes))
(byte-compile 'n-primes)

(comment
 (gcmh-time (setq 100k-primes (n-primes 100000))) ; 212.933452947
 (seq-reduce #'max 100k-primes 0) ; 1299709 oof, not yet at 2 million
                                  ; Trying optimization where we increment by 2
 (gcmh-time (setq 100k-primes (n-primes 100000))) ; 193.509257283 - minor savings
 (gcmh-time (setq 150k-primes (n-primes 150000))) ; 495.608933977
 (seq-max 150k-primes) ; Whew, just over 2 million
 (seq-reduce #'+ (seq-filter (lambda (n) (<= n 2000000)) 150k-primes) 0) ; 142913828922 correct
 )

;; P11
;; What is the largest product of four lined-up numbers (up, down, left. right, diagonal) in the big grid?

(setq p11-grid
      [[08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08]
       [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00]
       [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65]
       [52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91]
       [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
       [24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50]
       [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
       [67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21]
       [24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
       [21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95]
       [78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92]
       [16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57]
       [86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58]
       [19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40]
       [04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66]
       [88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69]
       [04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36]
       [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16]
       [20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54]
       [01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48]])

(defun euler-p11 ()
  (let ((max-adjacent-four '())
        (max-adjacent-product 0))
    (dotimes (col 20)
     (dotimes (row 20)
       (when-let ((_ (< col 17)) ; East
                  (elements (->> (list col (1+ col) (+ 2 col) (+ 3 col))
                                 (seq-map (lambda (col) (elt (elt p11-grid row) col)))))
                  (product  (apply #'* elements)))
         (if (> product max-adjacent-product)
           (setq max-adjacent-four elements
                 max-adjacent-product product)))
       (when-let ((_ (< row 17)) ; South
                  (elements (->> (list row (1+ row) (+ 2 row) (+ 3 row))
                                 (seq-map (lambda (row) (elt (elt p11-grid row) col)))))
                  (product  (apply #'* elements)))
         (if (> product max-adjacent-product)
           (setq max-adjacent-four elements
                 max-adjacent-product product)))
       (when-let ((_ (< col 17)) ; Southeast
                  (_ (< row 17))
                  (elements (->> (list 0 1 2 3)
                                 (seq-map (lambda (offset)
                                            (elt (elt p11-grid (+ offset row)) (+ offset col))))))
                  (product  (apply #'* elements)))
         (if (> product max-adjacent-product)
           (setq max-adjacent-four elements
                 max-adjacent-product product)))
       (when-let ((_ (< col 17)) ; Northeast
                  (_ (> row 2))
                  (elements (->> (list 0 1 2 3)
                                 (seq-map (lambda (offset)
                                            (elt (elt p11-grid (- row offset)) (+ offset col))))))
                  (product  (apply #'* elements)))
         (if (> product max-adjacent-product)
           (setq max-adjacent-four elements
                 max-adjacent-product product)))))

    (list max-adjacent-four max-adjacent-product)))

(comment
  (euler-p11) ; correct => ((87 97 94 89) 70600674)
 )

;; P12
;; Smallest triangle (sum i from 1 to n) number with over five hundred divisors

;; This is too slow, even with byte compilation
(defun has-501-divisors-p (n)
  (let ((max-divisor (floor (/ n 2)))
        (divisor-count 0)
        (current-divisor 1))
    (while (and (< divisor-count 501) (<= current-divisor max-divisor))
      (if (= 0 (% n current-divisor))
          (setq divisor-count (1+ divisor-count)))
      (setq current-divisor (1+ current-divisor)))
    (= 501 divisor-count)))
(byte-compile 'has-501-divisors-p)

; We take advantage of the fact that every divisor has a pair
; These pairs are around the square root
; This should let us check for 500 divisors in sqrt(n) time rather than n / 2
(defun has-501-divisors-by-sqrt-p (n)
  (let ((divisor-fulcrum (floor (sqrt n)))
        (divisor-count 0)
        (current-divisor 1))
    (if (= 0 (% n divisor-fulcrum))
        (setq divisor-count 1))
    (while (and (< divisor-count 501) (< current-divisor divisor-fulcrum))
      (if (= 0 (% n current-divisor))
          (setq divisor-count (+ 2 divisor-count)))
      (setq current-divisor (1+ current-divisor)))
    (message "Count: %d" divisor-count)
    (>= divisor-count 501)))
(byte-compile 'has-501-divisors-by-sqrt-p)

(defun euler-p12 ()
  (let ((triangle-num 1)
        (natural-num 2))
    (while (not (has-501-divisors-by-sqrt-p triangle-num))
      (setq triangle-num (+ triangle-num natural-num)
            natural-num (1+ natural-num))
      (message "At natural number %d and triangle number %d" natural-num triangle-num))
    triangle-num))
(byte-compile 'euler-p12)

(comment
 (has-501-divisors-p 12)
 (seq-reduce #'* (number-sequence 1 501) 1)
 (has-501-divisors-p (seq-reduce #'* (number-sequence 1 501) 1)) ; => t, pretty quick
 (euler-p12) ;
 (has-501-divisors-by-sqrt-p 76576500) ; Cheating a bit, returns true
 (has-501-divisors-p 76576500)
 (euler-p12) ; 76576500 works very speedily
 )

;; P13
;; Work out the ten digits of the sum of one hundred 50-digit numbers
;; Approach: The lower digits don't affect the first ten digits, so we can truncate them
;; But, where to truncate?
;; Elisp has native support for arbitrary-precision bignums, so let's just try to add them

(setq p13-numbers
      '(37107287533902102798797998220837590246510135740250
        46376937677490009712648124896970078050417018260538
        74324986199524741059474233309513058123726617309629
        91942213363574161572522430563301811072406154908250
        23067588207539346171171980310421047513778063246676
        89261670696623633820136378418383684178734361726757
        28112879812849979408065481931592621691275889832738
        44274228917432520321923589422876796487670272189318
        47451445736001306439091167216856844588711603153276
        70386486105843025439939619828917593665686757934951
        62176457141856560629502157223196586755079324193331
        64906352462741904929101432445813822663347944758178
        92575867718337217661963751590579239728245598838407
        58203565325359399008402633568948830189458628227828
        80181199384826282014278194139940567587151170094390
        35398664372827112653829987240784473053190104293586
        86515506006295864861532075273371959191420517255829
        71693888707715466499115593487603532921714970056938
        54370070576826684624621495650076471787294438377604
        53282654108756828443191190634694037855217779295145
        36123272525000296071075082563815656710885258350721
        45876576172410976447339110607218265236877223636045
        17423706905851860660448207621209813287860733969412
        81142660418086830619328460811191061556940512689692
        51934325451728388641918047049293215058642563049483
        62467221648435076201727918039944693004732956340691
        15732444386908125794514089057706229429197107928209
        55037687525678773091862540744969844508330393682126
        18336384825330154686196124348767681297534375946515
        80386287592878490201521685554828717201219257766954
        78182833757993103614740356856449095527097864797581
        16726320100436897842553539920931837441497806860984
        48403098129077791799088218795327364475675590848030
        87086987551392711854517078544161852424320693150332
        59959406895756536782107074926966537676326235447210
        69793950679652694742597709739166693763042633987085
        41052684708299085211399427365734116182760315001271
        65378607361501080857009149939512557028198746004375
        35829035317434717326932123578154982629742552737307
        94953759765105305946966067683156574377167401875275
        88902802571733229619176668713819931811048770190271
        25267680276078003013678680992525463401061632866526
        36270218540497705585629946580636237993140746255962
        24074486908231174977792365466257246923322810917141
        91430288197103288597806669760892938638285025333403
        34413065578016127815921815005561868836468420090470
        23053081172816430487623791969842487255036638784583
        11487696932154902810424020138335124462181441773470
        63783299490636259666498587618221225225512486764533
        67720186971698544312419572409913959008952310058822
        95548255300263520781532296796249481641953868218774
        76085327132285723110424803456124867697064507995236
        37774242535411291684276865538926205024910326572967
        23701913275725675285653248258265463092207058596522
        29798860272258331913126375147341994889534765745501
        18495701454879288984856827726077713721403798879715
        38298203783031473527721580348144513491373226651381
        34829543829199918180278916522431027392251122869539
        40957953066405232632538044100059654939159879593635
        29746152185502371307642255121183693803580388584903
        41698116222072977186158236678424689157993532961922
        62467957194401269043877107275048102390895523597457
        23189706772547915061505504953922979530901129967519
        86188088225875314529584099251203829009407770775672
        11306739708304724483816533873502340845647058077308
        82959174767140363198008187129011875491310547126581
        97623331044818386269515456334926366572897563400500
        42846280183517070527831839425882145521227251250327
        55121603546981200581762165212827652751691296897789
        32238195734329339946437501907836945765883352399886
        75506164965184775180738168837861091527357929701337
        62177842752192623401942399639168044983993173312731
        32924185707147349566916674687634660915035914677504
        99518671430235219628894890102423325116913619626622
        73267460800591547471830798392868535206946944540724
        76841822524674417161514036427982273348055556214818
        97142617910342598647204516893989422179826088076852
        87783646182799346313767754307809363333018982642090
        10848802521674670883215120185883543223812876952786
        71329612474782464538636993009049310363619763878039
        62184073572399794223406235393808339651327408011116
        66627891981488087797941876876144230030984490851411
        60661826293682836764744779239180335110989069790714
        85786944089552990653640447425576083659976645795096
        66024396409905389607120198219976047599490197230297
        64913982680032973156037120041377903785566085089252
        16730939319872750275468906903707539413042652315011
        94809377245048795150954100921645863754710598436791
        78639167021187492431995700641917969777599028300699
        15368713711936614952811305876380278410754449733078
        40789923115535562561142322423255033685442488917353
        44889911501440648020369068063960672322193204149535
        41503128880339536053299340368006977710650566631954
        81234880673210146739058568557934581403627822703280
        82616570773948327592232845941706525094512325230608
        22918802058777319719839450180888072429661980811197
        77158542502016545090413245809786882778948721859617
        72107838435069186155435662884062257473692284509516
        20849603980134001723930671666823555245252804609722
        53503534226472524250874054075591789781264330331690))

(defun euler-p13 ()
  (--> (apply #'+ p13-numbers)
       (number-to-string it)
       (substring it 0 11)))

(comment
 p13-numbers
 (euler-p13) ; "55373762303" correct
 )

;; P14
;; Starting number for Collatz sequence under one million producing the longest chain

;; Naive: Calculate chain length for each number, check one by one
;; Likely will need to memoize results, but let's see how far we get here

(defun collatz-length (n)
  (let ((steps 0))
    (while (> n 1)
      (setq steps (1+ steps)
            n (if (= 0 (% n 2))
                  (/ n 2)
                (+ 1 (* 3 n)))))
    (1+ steps)))
(byte-compile 'collatz-length)

(defun euler-p14 ()
  (let ((longest-starting-number 0)
        (longest-sequence 0))
    (dotimes (i 1000000)
     ; (message "Checking: %d" i)
      (let ((sequence-length (collatz-length i)))
        (if (> sequence-length longest-sequence)
            (setq longest-sequence sequence-length
                  longest-starting-number i))))
    (list longest-starting-number longest-sequence)))
(byte-compile 'euler-p14)

(defun euler-p14-memo ()
  (let ((sequence-lengths (make-hash-table)))
    (puthash 0 0 sequence-lengths)
    (puthash 1 1 sequence-lengths)
    (dotimes (i 1000000)
     ; (message "Checking %d" i) ; This adds a LOT of time to the execution
      (let (steps '())
        (while (not (gethash i sequence-lengths))
          (push i steps)
          (setq i (if (= 0 (% i 2))
                      (/ i 2)
                    (+ 1 (* 3 i)))))
        (let ((known-length (gethash i sequence-lengths)))
          (-each-indexed steps (lambda (i n) (puthash n (+ known-length (1+ i)) sequence-lengths))))))
    (let ((max-length 0)
          (max-length-start 0))
      (maphash (lambda (start length)
                 (when (> length max-length)
                   (setq max-length length
                         max-length-start start))) sequence-lengths)
      (list max-length-start max-length))))
(byte-compile 'euler-p14-memo)

(comment
 (collatz-length 13)
 (euler-p14) ; (837799 525) Correct
 (euler-p14-memo) ; (837799 525)
 (gcmh-time (euler-p14-memo)) ; 3.809513346
 (gcmh-time (euler-p14)) ; 10.652709481
 )

;; P15
;; Lattice paths
;; By noting that the number of paths for a position is the sum of paths
;; for the positions above and to the left of it, this naturally has
;; a dynamic programming solution. Recursive expression might work if
;; memoized

(defun grid-create (cols rows)
  (vector rows (make-vector (* cols rows) nil)))
(defun grid-row-size (grid)
  (elt grid 0))
(defun grid-data (grid)
  (elt grid 1))
(defun grid-get (grid col row)
  (elt (grid-data grid) (+ (* (grid-row-size grid) row) col)))
(defun grid-set (grid col row value)
  (aset (grid-data grid) (+ (* (grid-row-size grid) row) col) value))

(defun euler-p15 ()
  (let ((path-counts (grid-create 21 21)))
    (dotimes (row 21)
      (dotimes (col 21)
        (cond ((= 0 row) (grid-set path-counts col row 1))
              ((= 0 col) (grid-set path-counts col row
                               (grid-get path-counts col (1- row))))
               (t        (grid-set path-counts col row
                                   (+ (grid-get path-counts (1- col) row)
                                      (grid-get path-counts col      (1- row))))))))
    (grid-get path-counts 20 20)))

(comment
 (setq test-grid (grid-create 10 10))
 (grid-get test-grid 5 5)
 (grid-set test-grid 5 5 123)
 (grid-get test-grid 5 5)
 (euler-p15) ; 137846528820 Correct
 )

;; P16
;; Sum of digits of 2^1000

(defun euler-p16 ()
  (--> (expt 2 1000)
       (number-to-string it)
       (string-to-list it)
       (seq-map #'char-to-string it)
       (seq-map #'string-to-number it)
       (apply #'+ it)))

(comment
 (euler-p16) ; 1366 bignums are great, makes this very easy
 )
