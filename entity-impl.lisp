;;;; entity-impl.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-multiagent-system)

(deftype entity () '(function (symbol * &rest *) *))

(defun type-definition (entity-type)
  `(deftype ,entity-type () 'entity))

;;;----------------------------------------------------------------------------

(deftype missing-value () 'symbol)
(alexandria:define-constant +no-value+ '#.(gensym "NO-VALUE") 
                            :test (constantly t))

(defmacro no-value-p (value)
  `(eq ,value +no-value+))

;;-----------------------------------------------------------------------------

(defun append-aux-to-lambda-list (lambda-list &rest specifiers)
  (append lambda-list 
          (unless (member '&aux lambda-list :test #'eq)
            `(&aux))
          specifiers))

