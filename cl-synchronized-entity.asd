;;;; cl-synchronized-entity.asd
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(asdf:defsystem #:cl-synchronized-entity
  :description "A more flexible kind of structures, \
designed for multithreaded applications."
  :author "Ivan Podmazov"
  :license  "MIT"
  :version "1.0.0"
  :depends-on (#:alexandria #:bordeaux-threads)
  :serial t
  :components 
  ((:static-file "README.md")
   (:file "package")
   (:file "accessor-utils" :depends-on ("package"))
   (:file "accessor" :depends-on ("accessor-utils"))
   (:file "rw-lock" :depends-on ("package"))
   (:file "entity" :depends-on ("accessor" "rw-lock"))))

