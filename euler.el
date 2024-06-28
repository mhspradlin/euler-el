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

(comment
 (is-palindrome? 100)
 (is-palindrome? 101)
 (-iterate #'1+ 100 900)
 (euler-p4) ; => 906609

)
