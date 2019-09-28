;;;; package.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(defpackage #:cl-multiagent-system
  (:use #:cl)
  (:nicknames #:mas #:cl-mas)
  (:export :*accessor-name-fn* :*constructor-name-fn* 
           :missing-value :+no-value+ :no-value-p
           :synchronized-entity :define-synchronized-entity :self 
           :append-aux-to-lambda-list

           :*default-queue-empty-value* :queue :queue-empty-value
           :queue-empty-p :queue-front :queue-back 
           :queue-push :queue-pop :queue-pop-all

           :define-agent :running-p :start :stop :kill
           :htable :make-htable :htable-datum 
           :htable-read-data :htable-write-data
           :dagent :make-dagent :dagent-type-id :dagent-instance-id 
           :dagent-loop-fn :dagent-start-fn :dagent-stop-fn :dagent-route-fn
           :dagent-data :dagent-message :dagent-forward-message 
           :dagent-running-p :dagent-start :dagent-stop :dagent-kill 
           :dagent-datum :define-dagent-datum-accessor 
           :dagent-read-data :dagent-write-data))

