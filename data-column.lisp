;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:data-omnivore.data-column
  (:use #:cl
        #:anaphora
        #:data-omnivore.decimal-omnivore
        #:data-omnivore.string-table
        #:let-plus)
  (:export
   #:data-column
   #:data-column-add
   #:data-column-counts
   #:data-column-vector))

(in-package #:data-omnivore.data-column)



(defclass data-column ()
  ((reverse-elements
    :initform nil
    :type list)
   (default-float-format
    :initarg :default-float-format
    :type symbol)
   (float-count
    :initform 0
    :type non-negative-integer)
   (integer-count
    :initform 0
    :type non-negative-integer)
   (integer-min
    :initform nil
    :type integer)
   (integer-max
    :initform nil
    :type integer)
   (map-count
    :initform 0
    :type non-negative-integer)
   (map-table
    :initarg :map-table
    :type string-table)
   (string-count
    :initform 0
    :type non-negative-integer)
   (string-table
    :initform (string-table)
    :type string-table)))

(defun data-column (&key map-alist (default-float-format 'double-float))
  (make-instance 'data-column
                 :default-float-format default-float-format
                 :map-table (aprog1 (string-table)
                              (loop for (string . value) in map-alist
                                    do (check-type value
                                                   (not (or integer float string)))
                                       (string-table-add it string value)))))

(defun data-column-add (data-column string)
  (let+ (((&slots-r/o default-float-format map-table string-table) data-column)
         ((&slots float-count integer-count integer-min integer-max
                  map-count string-count) data-column)
         (element (handler-case (prog1 (string-table-lookup map-table string)
                                  (incf map-count))
                    (string-table-not-found ()
                      (handler-case (aprog1 (parse-real string
                                                        :e-float default-float-format)
                                      (if (integerp it)
                                          (progn  (incf integer-count)
                                                  (if integer-min
                                                      (progn
                                                        (alexandria:maxf
                                                         integer-min it)
                                                        (alexandria:maxf
                                                         integer-max it))
                                                      (setf integer-min it
                                                            integer-max it)))
                                          (incf float-count)))
                        (parse-rational-error ()
                          (prog1 (string-table-intern string-table string string)
                            (incf string-count))))))))
    (push element (slot-value data-column 'reverse-elements))
    element))

(defun data-column-counts (data-column)
  "Return the counts."
  ;; QUESTION may remove this function, data-column-vector should take care of conversions?
  (let+ (((&slots-r/o float-count integer-count map-count string-count)
          data-column))
    (list :float-count float-count
          :integer-count integer-count
          :map-count map-count
          :string-count string-count)))

(defun data-column-vector (data-column)
  "Return the collected elements as a vector."
  (let+ (((&slots-r/o float-count integer-count integer-min integer-max
                      map-count string-count)
          data-column)
         (element-type
          (cond
            ((and (= 0 float-count map-count string-count)
                  (plusp integer-count))
             (cond
               ((<= 0 integer-min integer-max 1) 'bit)
               (t t)))
            (t t))))
    (coerce (reverse (slot-value data-column 'reverse-elements))
            `(simple-array ,element-type (*)))))
