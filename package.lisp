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
           :agent :make-agent :agent-type-id :agent-instance-id :agent-loop-fn
           :agent-start-fn :agent-stop-fn :agent-running-p 
           :agent-message :agent-datum :agent-read-data :agent-write-data
           :agent-start :agent-stop :agent-kill 
           :define-agent-datum-accessor
           :message :make-message 
           :message-addresser :message-addressee :message-swap-addresses 
           :message-datum :message-read-data :message-write-data 
           :define-message-datum-accessor))

