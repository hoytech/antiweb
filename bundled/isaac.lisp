;; isaac.lisp (C) May 2008 Doug Hoyte, HCSW
;; BSD license: you can do anything you want with it (but no warranty).
;;
;; Optimised Common Lisp implementation of Bob Jenkins' ISAAC-32 Algorithm:
;; Indirection, Shift, Accumulate, Add, and Count. More details and
;; the C reference implementations can be found here:
;;
;; ISAAC: a fast cryptographic random number generator
;; http://burtleburtle.net/bob/rand/isaacafa.html
;;
;; This lisp implementation is roughly as fast as Jenkins' optimised rand.c
;; when compiled with a good native-code lisp compiler. It also performs
;; well when byte-code compiled.
;;
;;
;; USAGE:
;;
;; First, create an isaac context. There are three functions that do this:
;;
;;   isaac:init-kernel-seed => <isaac context>
;;     *RECOMMENDED* Seeds with values from /dev/arandom on BSD
;;     or /dev/urandom on Linux. Reads 1024 bytes from the device.
;;
;;   isaac:init-common-lisp-random-seed => <isaac context>
;;     Seeds with values from your Common Lisp implementation's
;;     random function. Consumes 256 32-bit values from #'random.
;;
;;   isaac:init-null-seed => <isaac context>
;;     Seeds with all 0s. Always results in the same stream.
;;     For comparing with Jenkins' reference implementations.
;;
;; These are functions you can pass an isaac context to. They will modify
;; the isaac context and return a random value:
;;
;;   isaac:rand32 <isaac context> => <random 32-bit value>
;;     Uses the ISAAC-32 algorithm to generate a new random value.
;;
;;   isaac:rand-bits <isaac context> <N> => <random N-bit value>
;;     Uses the ISAAC-32 algorithm to generate random values between
;;     0 and (1- (expt 2 N)). This function always consumes one or more
;;     ISAAC-32 words. Note that the N parameter is different from
;;     the CL random function parameter.   Examples:
;;       (isaac:rand-bits ctx 1) => [0,1] (consumes 1 ISAAC-32 word)
;;       (isaac:rand-bits ctx 2) => [0,1,2,3] (ditto)
;;       (isaac:rand-bits ctx 3) => [0,1,2,3,4,5,6,7] (ditto)
;;       (isaac:rand-bits ctx 32) => [0,1,...,(1- (expt 2 32))] (ditto)
;;       (isaac:rand-bits ctx 33) => [0,1,...,(1- (expt 2 33))] (consumes 2 words)
;;       (isaac:rand-bits ctx 512) => [0,1,...,(1- (expt 2 512))] (consumes 16 words)
;;
;;
;; QUICK RECIPE:
;;
;; Generate a 128-bit session ID as a 0-padded hexadecimal string:
;;   (compile-file "isaac.lisp")
;;   (load "isaac")
;;   (defvar my-isaac-ctx (isaac:init-kernel-seed))
;;   (format nil "~32,'0x" (isaac:rand-bits my-isaac-ctx 128))
;;     => "078585213B0EF01B1B9BECB291EF38F0"
;;
;;
;; FAQ:
;;     Q) My Common Lisp implementation already uses the Mersenne Twister,
;;        what are the advantages of ISAAC?
;;
;;     A1) The Mersenne Twister is not a cryptographic PRNG. This means that it
;;         is possible for someone to predict future values based on previously
;;         observed values (just over 600 of them). As such, MT is particularly
;;         undesirable for things like web session IDs. You can still use MT for
;;         crypto, but you must use a cryptographic hash function on the MT output.
;;     A2) isaac.lisp appears to be roughly as fast as the Mersenne Twister #'random
;;         of CMUCL 19d on x86 before even considering the above-mentioned hash
;;         function overhead requirement of MT.
;;     A3) isaac.lisp is not implemented as an x86 VOP like CMUCL's Mersenne
;;         Twister, but instead in 100% standard ANSI Common Lisp (except for the
;;         kernel seed interface). This should mean comparable performance on all
;;         architectures targeted by your lisp compiler. The non-x86 MT
;;         implementation is apparently an order-of-magnitude slower.
;;
;;     Q) How "random" can I expect these values to be?
;;
;;     A) Very. From Bob Jenkins' website: "Cycles are guaranteed to be at least
;;        (expt 2 40) values long, and they are (expt 2 8295) values long on
;;        average. The results are uniformly distributed, unbiased, and unpredictable
;;        unless you know the seed. [...] Why not use RC4? RC4 is three times slower,
;;        more biased, has a shorter minimum and average cycle length, and is
;;        proprietary. No way is known to break either RC4 or ISAAC; both are immune
;;        to Gaussian elimination."
;;
;;        Note that there is a $1000 prize you can win from Jenkins if you find
;;        a flaw in ISAAC (but all flaws in isaac.lisp are of course mine).


(defpackage #:isaac
   (:use :cl)
   (:export #:init-null-seed
            #:init-kernel-seed
            #:init-common-lisp-random-seed
            #:rand32
            #:rand-bits))

(in-package #:isaac)


(defstruct isaac-ctx
  (randcnt 0 :type (unsigned-byte 32))
  (randrsl (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0)
           :type (simple-array (unsigned-byte 32) (256)))
  (randmem (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0)
           :type (simple-array (unsigned-byte 32) (256)))
  (a 0 :type (unsigned-byte 32))
  (b 0 :type (unsigned-byte 32))
  (c 0 :type (unsigned-byte 32)))



(defun generate-next-isaac-block (ctx)
  (declare (optimize (speed 3) (safety 0)))
  (incf (isaac-ctx-c ctx))
  (incf (isaac-ctx-b ctx) (isaac-ctx-c ctx))
  (loop for i from 0 below 256 do
    (setf (isaac-ctx-a ctx)
          (logxor (isaac-ctx-a ctx)
                  (logand #xFFFFFFFF
                    (the (unsigned-byte 32)
                      (ash (isaac-ctx-a ctx)
                           (ecase (logand i 3)
                             ((0) 13)
                             ((1) -6)
                             ((2) 2)
                             ((3) -16)))))))
    (setf (isaac-ctx-a ctx)
          (logand #xFFFFFFFF
                  (+ (isaac-ctx-a ctx)
                     (aref (isaac-ctx-randmem ctx) (logand (+ i 128) #xFF)))))
    (let* ((x (aref (isaac-ctx-randmem ctx) i))
           (y (logand #xFFFFFFFF
                      (+ (aref (isaac-ctx-randmem ctx) (logand (ash x -2) #xFF))
                         (isaac-ctx-a ctx)
                         (isaac-ctx-b ctx)))))
      (setf (aref (isaac-ctx-randmem ctx) i) y)
      (setf (isaac-ctx-b ctx)
            (logand #xFFFFFFFF
                    (+ (aref (isaac-ctx-randmem ctx) (logand (ash y -10) #xFF)) x)))
      (setf (aref (isaac-ctx-randrsl ctx) i) (isaac-ctx-b ctx)))))



(defun rand32 (ctx)
  (let ((c (isaac-ctx-randcnt ctx)))
    (declare (optimize (speed 3) (safety 0)))
    (decf (isaac-ctx-randcnt ctx))
    (if (zerop c)
      (progn
        (generate-next-isaac-block ctx)
        (setf (isaac-ctx-randcnt ctx) 255)
        (aref (isaac-ctx-randrsl ctx) 255))
      (aref (isaac-ctx-randrsl ctx) (isaac-ctx-randcnt ctx)))))


(defun rand-bits (ctx n)
  (let ((v 0))
    (loop while (> n 0) do
      (setq v (logior (ash v (min n 32))
                      (logand (1- (ash 1 (min n 32)))
                              (rand32 ctx))))
      (decf n (min n 32)))
    v))




(defmacro incf-wrap32 (a b)
  `(setf ,a (logand #xFFFFFFFF (+ ,a ,b))))

(defmacro mix (a b c d e f g h)
  `(progn
     (setf ,a (logxor ,a (logand #xFFFFFFFF (ash ,b 11)))) (incf-wrap32 ,d ,a) (incf-wrap32 ,b ,c)
     (setf ,b (logxor ,b (logand #xFFFFFFFF (ash ,c -2)))) (incf-wrap32 ,e ,b) (incf-wrap32 ,c ,d)
     (setf ,c (logxor ,c (logand #xFFFFFFFF (ash ,d 8)))) (incf-wrap32 ,f ,c) (incf-wrap32 ,d ,e)
     (setf ,d (logxor ,d (logand #xFFFFFFFF (ash ,e -16)))) (incf-wrap32 ,g ,d) (incf-wrap32 ,e ,f)
     (setf ,e (logxor ,e (logand #xFFFFFFFF (ash ,f 10)))) (incf-wrap32 ,h ,e) (incf-wrap32 ,f ,g)
     (setf ,f (logxor ,f (logand #xFFFFFFFF (ash ,g -4)))) (incf-wrap32 ,a ,f) (incf-wrap32 ,g ,h)
     (setf ,g (logxor ,g (logand #xFFFFFFFF (ash ,h 8)))) (incf-wrap32 ,b ,g) (incf-wrap32 ,h ,a)
     (setf ,h (logxor ,h (logand #xFFFFFFFF (ash ,a -9)))) (incf-wrap32 ,c ,h) (incf-wrap32 ,a ,b)))



(defun scramble (ctx)
  (let (a b c d e f g h)
    (setf a #x9e3779b9  b a  c a  d a  e a  f a  g a  h a) ; golden ratio

    (loop for i from 0 below 4 do
      (mix a b c d e f g h))

    ;; Pass #1
    (loop for i from 0 below 256 by 8 do
      (incf-wrap32 a (aref (isaac-ctx-randrsl ctx) (+ i 0)))
      (incf-wrap32 b (aref (isaac-ctx-randrsl ctx) (+ i 1)))
      (incf-wrap32 c (aref (isaac-ctx-randrsl ctx) (+ i 2)))
      (incf-wrap32 d (aref (isaac-ctx-randrsl ctx) (+ i 3)))
      (incf-wrap32 e (aref (isaac-ctx-randrsl ctx) (+ i 4)))
      (incf-wrap32 f (aref (isaac-ctx-randrsl ctx) (+ i 5)))
      (incf-wrap32 g (aref (isaac-ctx-randrsl ctx) (+ i 6)))
      (incf-wrap32 h (aref (isaac-ctx-randrsl ctx) (+ i 7)))
      (mix a b c d e f g h)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 0)) a)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 1)) b)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 2)) c)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 3)) d)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 4)) e)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 5)) f)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 6)) g)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 7)) h))

    ;; Pass #2
    (loop for i from 0 below 256 by 8 do
      (incf-wrap32 a (aref (isaac-ctx-randmem ctx) (+ i 0)))
      (incf-wrap32 b (aref (isaac-ctx-randmem ctx) (+ i 1)))
      (incf-wrap32 c (aref (isaac-ctx-randmem ctx) (+ i 2)))
      (incf-wrap32 d (aref (isaac-ctx-randmem ctx) (+ i 3)))
      (incf-wrap32 e (aref (isaac-ctx-randmem ctx) (+ i 4)))
      (incf-wrap32 f (aref (isaac-ctx-randmem ctx) (+ i 5)))
      (incf-wrap32 g (aref (isaac-ctx-randmem ctx) (+ i 6)))
      (incf-wrap32 h (aref (isaac-ctx-randmem ctx) (+ i 7)))
      (mix a b c d e f g h)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 0)) a)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 1)) b)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 2)) c)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 3)) d)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 4)) e)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 5)) f)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 6)) g)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 7)) h))

    (generate-next-isaac-block ctx)
    (setf (isaac-ctx-randcnt ctx) 256)

    ctx))



(defun init-kernel-seed ()
  (let ((ctx (make-isaac-ctx)))
    (or
      (ignore-errors
        (with-open-file (r "/dev/arandom" :direction :input :element-type '(unsigned-byte 32))
          #1=(loop for i from 0 below 256 do
               (setf (aref (isaac-ctx-randrsl ctx) i) (read-byte r)))
          t))
      (ignore-errors
        (with-open-file (r "/dev/urandom" :direction :input :element-type '(unsigned-byte 32))
          #1#
          t))
      (error "couldn't open /dev/arandom or /dev/urandom"))
    (scramble ctx)))

(defun init-common-lisp-random-seed ()
  (let ((ctx (make-isaac-ctx)))
    (loop for i from 0 below 256 do
      (setf (aref (isaac-ctx-randrsl ctx) i) 
            (random (ash 1 32))))
    (scramble ctx)))

(defun init-null-seed ()
  (let ((ctx (make-isaac-ctx)))
    (scramble ctx)))




;; Output is the same as Jenkins' randvect.txt
(defun jenkins-output (filename)
  (let ((ctx (init-null-seed)))
    (with-open-file (o filename :direction :output :if-exists :supersede)
      (loop for i from 0 below 2 do
        (generate-next-isaac-block ctx)
        (loop for j from 0 below 256 do
          (format o "~(~8,'0x~)" (aref (isaac-ctx-randrsl ctx) j))
          (if (= 7 (logand j 7)) (terpri o)))))))
