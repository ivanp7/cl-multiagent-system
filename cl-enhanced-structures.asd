;;;; cl-enhanced-structures.asd
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(asdf:defsystem #:cl-enhanced-structures
  :description "A more flexible kind of structures."
  :author "Ivan Podmazov"
  :license  "MIT"
  :version "1.0.0"
  :depends-on (#:alexandria #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "structure" :depends-on ("package"))))

