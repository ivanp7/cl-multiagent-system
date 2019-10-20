;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:cl-multiagent-system
  (:use #:cl)
  (:nicknames #:mas #:cl-mas)
  (:export :*accessor-name-fn* :*constructor-name-fn* 
           :missing-value :+no-value+ :no-value-p
           :entity :define-entity :self 
           :entity-accessor

           :define-thread :running-p :start :stop :destroy

           :*default-queue-empty-value* :queue :queue-empty-value
           :queue-empty-p :queue-front :queue-back 
           :queue-push :queue-pop :queue-pop-all

           :registry :make-registry 
           :registry-default-value :registry-value-test 
           :registry-entry :registry-keys :registry-entry-present-p 
           :registry-map-entries 
           :registry-add-entry :registry-del-entry

           :*agent-loop-fn-ctor* :*agent-start-fn-ctor* :*agent-stop-fn-ctor*
           :*agent-data-ctor*

           :agent :make-agent :agent-instance-id :agent-role-id
           :agent-loop-fn :agent-start-fn :agent-stop-fn :agent-data
           :agent-host-thread :agent-running-p :agent-start :agent-stop
           :agent-message

           :multiagent-thread :make-multiagent-thread 
           :multiagent-thread-running-p :multiagent-thread-start
           :multiagent-thread-stop :multiagent-thread-destroy
           :multiagent-thread-map-agents

           :messenger :make-messenger :messenger-site-registry
           :messenger-agent-registry :messenger-sender-registry
           :messenger-send

           :make-stream-sender :make-stream-receiver))

