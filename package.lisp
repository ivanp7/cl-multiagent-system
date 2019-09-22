;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:cl-synchronized-entity
  (:use #:cl)
  (:nicknames #:cl-se #:cl-sentity)
  (:export :*accessor-name-fn* :*constructor-name-fn* 
           :define-synchronized-entity :self :+no-value+
           :*default-queue-empty-value* :queue :queue-empty-value
           :queue-empty-p :queue-front :queue-back :queue-push :queue-pop
           :agent :agent-type-id :agent-instance-id :agent-function
           :agent-start-fn :agent-stop-fn :agent-running-p 
           :agent-message :agent-data :agent-start :agent-stop :agent-kill
           :define-agent-data-accessor))

