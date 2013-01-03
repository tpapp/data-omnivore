(asdf:defsystem #:data-omnivore
  :version "0"
  :description "Common Lisp library for reading in data from text files (eg CSV)."
  :maintainer "Tamas Papp <tkpapp@gmail.com>"
  :author "Tamas Papp <tkpapp@gmail.com>"
  :licence "BOOST"
  :depends-on (#:alexandria
               #:anaphora
               #:cl-csv
               #:cl-data-frame
               #:let-plus)
  :serial t
  :components ((:file "decimal-omnivore")
               (:file "string-table")
               (:file "data-column")))

(asdf:defsystem #:data-omnivore-tests
  :version "0"
  :description "Unit tests for DATA-OMNIVORE."
  :maintainer "Tamas Papp <tkpapp@gmail.com>"
  :author "Tamas Papp <tkpapp@gmail.com>"
  :depends-on (#:data-omnivore
               #:clunit)
  :serial t
  :components ((:file "tests")))
