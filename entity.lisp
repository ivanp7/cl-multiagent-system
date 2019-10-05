;;;; entity.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-multiagent-system)

(defmacro define-entity (entity-type lambda-list 
                         (&key declarations initialization) 
                         &rest accessors)
  (setf accessors (prepare-accessors entity-type accessors))
  `(progn
     ,(type-definition entity-type)
     ,@(accessors-definitions entity-type accessors)
     ,(constructor-definition entity-type lambda-list 
                              declarations initialization accessors)))

