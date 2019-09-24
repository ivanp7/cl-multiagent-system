;;;; cl-multiagent-system.asd
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(asdf:defsystem #:cl-multiagent-system
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
   (:file "entity-utils" :depends-on ("package"))
   (:file "entity-accessor" :depends-on ("entity-utils"))
   (:file "entity-lock" :depends-on ("package"))
   (:file "entity" :depends-on ("entity-accessor" "entity-lock"))
   (:file "queue" :depends-on ("package"))
   (:file "agent" :depends-on ("entity" "queue"))))

