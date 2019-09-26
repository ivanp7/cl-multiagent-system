;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:cl-multiagent-system
  (:use #:cl)
  (:nicknames #:mas #:cl-mas)
  (:export :*accessor-name-fn* :*constructor-name-fn* 
           :define-synchronized-entity :self 
           :missing-value :+no-value+ :no-value-p
           :*default-queue-empty-value* :queue :queue-empty-value
           :queue-empty-p :queue-front :queue-back 
           :queue-push :queue-pop :queue-pop-all
           :table :make-table :table-datum :table-read-data :table-write-data
           :agent :make-agent :agent-type-id :agent-instance-id :agent-loop-fn
           :agent-start-fn :agent-stop-fn :agent-running-p :agent-table
           :agent-message :agent-forward-message
           :agent-start :agent-stop :agent-kill 
           :define-agent-datum-accessor))

