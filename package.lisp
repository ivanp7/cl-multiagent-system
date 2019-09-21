;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:cl-synchronized-entity
  (:use #:cl)
  (:nicknames #:cl-se #:cl-sentity)
  (:export :*accessor-name-fn* :*constructor-name-fn* 
           :define-synchronized-entity :self))

