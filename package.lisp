;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:cl-multiagent-system
  (:use #:cl)
  (:nicknames #:mas #:cl-mas)
  (:export :*accessor-name-fn* :*constructor-name-fn* 
           :missing-value :+no-value+ :no-value-p
           :entity :define-entity :self 

           :define-thread :running-p :start :stop :destroy

           :*default-queue-empty-value* :queue :queue-empty-value
           :queue-empty-p :queue-front :queue-back 
           :queue-push :queue-pop :queue-pop-all

           :data-table :make-data-table :data-table-field 
           :data-table-read-fields :data-table-write-fields

           :*agent-loop-fn-ctor* :*agent-start-fn-ctor* :*agent-stop-fn-ctor*
           :*agent-data-table-ctor*

           :agent :make-agent :agent-instance-id :agent-role-id
           :agent-loop-fn :agent-start-fn :agent-stop-fn :agent-data-table
           :agent-host-thread :agent-running-p :agent-start :agent-stop
           :agent-message

           :multiagent-thread :make-multiagent-thread 
           :multiagent-thread-running-p :multiagent-thread-start
           :multiagent-thread-stop :multiagent-thread-destroy
           :multiagent-thread-map-agents))

