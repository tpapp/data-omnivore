;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:data-column.data-omnivore
  (:use #:cl
        #:anaphora
        #:decimal-omnivore.data-omnivore
        #:string-table.data-omnivore
        #:let-plus)
  (:nicknames #:data-column)
  (:export
   #:data-column
   #:make
   #:add
   #:float-count
   #:integer-count
   #:map-count
   #:string-count
   #:elements))

(in-package #:data-column)



(defconstant +data-column-min-extension+ 32
  "")

(defclass data-column ()
  ((elements
    :initform (make-array +data-column-min-extension+
                          :adjustable t
                          :fill-pointer 0)
    :type vector
    :reader elements)
   (float-count
    :initform 0
    :type non-negative-integer
    :reader float-count)
   (integer-count
    :initform 0
    :type non-negative-integer
    :reader integer-count)
   (map-count
    :initform 0
    :type non-negative-integer
    :reader map-count)
   (map-table
    :initarg :map-table
    :type string-table)
   (string-count
    :initform 0
    :type non-negative-integer
    :reader string-count)
   (string-table
    :initform (string-table)
    :type string-table)))

(defun make (&key map-alist)
  (make-instance 'data-column
                 :map-table (aprog1 (string-table)
                              (loop for (string . value) in map-alist
                                    do (check-type value
                                                   (not (or integer float string)))
                                       (string-table-add it string value)))))

(defun add (data-column string)
  (let+ (((&slots-r/o elements float-count integer-count
                      map-count map-table
                      string-count string-table) data-column)
         (element (handler-case (prog1 (string-table-lookup map-table string)
                                  (incf map-count))
                    (string-table-not-found ()
                      (handler-case (aprog1 (parse-real string)
                                      (if (integerp it)
                                          (incf integer-count)
                                          (incf float-count)))
                        (parse-rational-error ()
                          (prog1 (string-table-intern string-table string string)
                            (incf string-count))))))))
    (vector-push-extend element elements +data-column-min-extension+)
    element))
