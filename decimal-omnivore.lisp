;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:decimal-omnivore
  (:use #:cl #:anaphora #:let-plus)
  (:export
   #:parse-rational-error
   #:parse-rational
   #:parse-real))

(in-package #:decimal-omnivore)

;;; conditions

(define-condition parse-rational-error (error)
  ((string :initform :string)
   (message :initform :message))
  (:report (lambda (condition stream)
             (let+ (((&slots-r/o string message) condition))
               (format stream "Could not parse ~A as a real number: ~A."
                       string message)))))

(defun gobble-positive-integer (string start end)
  "If (SUBSEQ STRING START END) starts with a nonnegative integer (ie a
sequence of digits 0-9), return the integer and position at which it ends as
two values.

Otherwise, return NIL and 0.

START < END has to hold, END cannot be NIL.  Consequences are undefined when
START >= END."
  (let+ ((result 0)
         ((&flet done (index)
            (return-from gobble-positive-integer
              (if (eql index 0)
                  (values nil 0)
                  (values result index))))))
    (loop for index from start below end
          do (aif (digit-char-p (char string index))
                  (setf result (+ it (* result 10)))
                  (done index)))
    (done end)))

(defun gobble-integer (string start end)
  "If (SUBSEQ STRING START END) starts with an integer (ie an optional sign,
followed by a sequence of digits 0-9), return the integer and position at
which it ends as two values.

Otherwise, return NIL.

START < END has to hold, END cannot be NIL.  Consequences are undefined when
START >= END."
  (let ((first-char (char string start)))
    (case first-char
      ((#\+ #\-)
       (let+ (((&values value end)
               (gobble-positive-integer string (1+ start) end)))
         (if value
             (values (if (char= #\+ first-char)
                         value
                         (- value))
                     end)
             (error "no digits follow ~A" first-char))))
      (otherwise (gobble-positive-integer string start end)))))

(defparameter +exponent-chars+ "defslDEFSL")

(declaim (inline pow10))

(defun pow10 (power)
  "10 to the given power.  Always exact."
  (check-type power integer)
  (expt 10 power))

(defun parse-rational (string &key (start 0) (end nil)
                                   (exponent-chars +exponent-chars+))
  (let+ ((end (or end (length string)))
         ((&macrolet when-end (&body body)
            `(when (= start end)
               ,@body)))
         (sign 1)
         whole
         decimal-dot?
         fraction fraction-digits
         exponent exponent-char
         ((&flet report-error (message)
            (error 'parse-rational-error :string string
                                         :message message)))
         ((&flet make-real ()
            (let ((mantissa
                    (* sign
                       (cond
                         ((and whole fraction)
                          (+ whole (* fraction (pow10 (- fraction-digits)))))
                         (whole whole)
                         (fraction (* fraction (pow10 (- fraction-digits))))
                         (t (report-error "no digits around decimal dot"))))))
              (return-from parse-rational
                (values (if exponent
                            (* mantissa (pow10 exponent))
                            mantissa)
                        decimal-dot?
                        exponent-char))))))
    (assert (< start end))
    ;; sign
    (let ((maybe-sign (char string start)))
      (case maybe-sign
        (#\+ (incf start))
        (#\- (incf start)
         (setf sign -1))))
    (when-end (report-error "sign without digits"))
    ;; whole
    (setf (values whole start) (gobble-positive-integer string start end))

    (when-end (make-real))
    ;; fraction
    (when (char= (char string start) #\.)
      (setf decimal-dot? t)
      (incf start)
      (when-end (make-real))
      (let+ (((&values fraction% end%)
              (gobble-positive-integer string start end)))
        (when fraction%
          (setf fraction fraction%
                fraction-digits (- (or end% (length string)) start)
                start end%)))
      (when-end (make-real)))
    ;; exponent
    (let ((maybe-exponent-char (char string start)))
      (when (find maybe-exponent-char exponent-chars)
        (setf exponent-char maybe-exponent-char)
        (incf start)
        (when-end (report-error "no characters after exponent character"))
        (setf (values exponent start) (gobble-integer string start end))
        (unless exponent
          (report-error "missing exponent"))
        (when-end (make-real))))
    (when-end (make-real))
    (error 'malformed-real :string string :message "trailing junk")))

(defun parse-real (string &key (start 0) (end nil)
                               (s-float 'short-float)
                               (f-float 'single-float)
                               (d-float 'double-float)
                               (l-float 'long-float)
                               (e-float *read-default-float-format*))
  (let+ (((&values real decimal-dot? exponent-char)
          (parse-rational string :start start
                                 :end end
                                 :exponent-chars +exponent-chars+)))
    (if exponent-char
        (coerce real
                (ecase (char-downcase exponent-char)
                  (#\s s-float)
                  (#\f f-float)
                  (#\d d-float)
                  (#\l l-float)
                  (#\e e-float)))
        (if decimal-dot?
            (coerce real e-float)
            real))))
