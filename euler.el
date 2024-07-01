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
