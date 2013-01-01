;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:data-omnivore-tests
  (:use #:cl #:clunit #:decimal-omnivore #:let-plus)
  (:export
   #:run))

(in-package #:data-omnivore-tests)



;;; interface

(defsuite data-omnivore-tests ())

(defun run (&optional interactive?)
  (run-suite 'data-omnivore-tests :use-debugger interactive?))

;;; decimal-omnivore

(defsuite decimal-omnivore-tests (data-omnivore-tests))

(defun random-sign ()
  "Return a random sign (+,-) or an empty string."
  (ecase (random 3)
    (0 "")
    (1 "+")
    (2 "-")))

(defun random-signless (digits)
  "Random sequence of digits, representing an integer.  DIGITS suggests a total length for nonzero digits, but it is random, padded with a random number of zeroes.  For testing.  When digits is NIL, an empty string is returned."
  (if digits
      (progn
        (check-type digits (integer 1))
        (format nil "~v,'0D"
                (random (* 2 digits))
                (random (expt 10 digits))))
      ""))

(defun random-integer-string (digits)
  "Return (cons VALUE STRING) such that (PARSE-REAL STRING) is expected to return VALUE, which is an integer.

See RANDOM-SIGNLESS for the semantics of DIGITS, the result of (RANDOM-SIGN) is prepended."
  (let ((string (concatenate 'string (random-sign) (random-signless digits))))
    (cons (parse-integer string) string)))

(defun random-float-string (&key (exponent-char #\d)
                                     (whole-digits 6)
                                     (fraction-digits 6)
                                     (exponent-digits 2))
  "Return (cons VALUE STRING) such that (PARSE-REAL STRING) is expected to give VALUE.  Useful for testing.

STRING represents a number, randomly generated according to the following rules:

  - Whole and fractional parts are generated using RANDOM-SIGNLESS called with WHOLE-DIGITS and FRACTION-DIGITS, prepended by a random sign.

  - EXPONENT-DIGITS has similar semantics.  EXPONENT-CHAR is used before the exponent (if applicable)."
  (assert (or whole-digits fraction-digits) ()
          "No digits around the decimal dot.")
  (let+ (((&flet cat (&rest strings)
            (apply #'concatenate 'string strings)))
         (whole-sign (random-sign))
         (whole (random-signless whole-digits))
         (fraction (random-signless fraction-digits))
         (exponent (if exponent-digits
                       (format nil "~C~A~A"
                               exponent-char
                               (random-sign)
                               (random-signless exponent-digits))
                       ""))
         (string (cat whole-sign whole "." fraction exponent))
         (value (read-from-string
                 (concatenate 'string
                              (cat whole-sign
                                   (if whole-digits
                                       whole
                                       "0"))
                              "."
                              (if fraction-digits
                                  fraction
                                  "0")
                              exponent))))
    (cons value string)))

(defparameter *iterations* 10000
  "Number of iterations for random tests.")

(defmacro random-parse-test (form)
  "Evaluates FORM repeatedly, using the resuling (cons VALUE STRING) to test PARSE-REAL."
  `(loop repeat *iterations*
         do (let+ (((value . string) ,form))
              (assert-equalp value (parse-real string)
                value string))))

(deftest parse-real-test (decimal-omnivore-tests)
  (random-parse-test (random-float-string)))

(deftest parse-real-test-noexp (decimal-omnivore-tests)
  (random-parse-test (random-float-string :exponent-digits nil)))

(deftest parse-real-test-nowhole (decimal-omnivore-tests)
  (random-parse-test (random-float-string :whole-digits nil)))

(deftest parse-real-test-nofrac (decimal-omnivore-tests)
  (random-parse-test (random-float-string :fraction-digits nil)))

(deftest parse-real-test-integer (decimal-omnivore-tests)
  (random-parse-test (random-integer-string 5)))

(deftest parse-rational-errors (decimal-omnivore-tests)
  (assert-condition parse-rational-error (parse-rational ""))
  (assert-condition parse-rational-error (parse-rational "junk"))
  (assert-condition parse-rational-error (parse-rational "1..2"))
  (assert-condition parse-rational-error (parse-rational "  12  "))
  (assert-condition parse-rational-error (parse-rational "1.-2")))
