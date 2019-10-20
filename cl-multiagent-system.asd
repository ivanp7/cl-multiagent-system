;;;; cl-multiagent-system.asd
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(asdf:defsystem #:cl-multiagent-system
  :description "A multithreaded model of a multiagent system."
  :author "Ivan Podmazov"
  :license  "MIT"
  :version "1.0.0"
  :depends-on (#:alexandria #:bordeaux-threads #:green-threads)
  :serial t
  :components 
  ((:static-file "README.md")
   (:file "package")

   (:file "entity-impl" :depends-on ("package"))
   (:file "entity-impl-accessor-description" :depends-on ("package"))
   (:file "entity-impl-lock" :depends-on ("package"))

   (:file "entity-impl-accessors" 
    :depends-on ("entity-impl-accessor-description"))
   (:file "entity-impl-constructor" 
    :depends-on ("entity-impl-accessor-description" "entity-impl-lock"))

   (:file "entity" :depends-on ("entity-impl" "entity-impl-accessors"
                                "entity-impl-constructor"))
   (:file "thread" :depends-on ("entity-impl" "entity-impl-accessors"
                                "entity-impl-constructor"))
   (:file "queue" :depends-on ("package"))
   (:file "registry" :depends-on ("package"))
   (:file "agent" :depends-on ("thread" "queue" "registry"))
   (:file "messenger" :depends-on ("registry" "agent"))
   (:file "stream" :depends-on ("package"))))

