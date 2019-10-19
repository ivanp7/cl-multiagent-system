;;;; messenger.lisp

(in-package #:cl-multiagent-system)

(define-entity messenger
    (&key (site-registry (make-registry)) (agent-registry (make-registry))
          (sender-registry (make-registry)))
    (:declarations
      ((type registry site-registry agent-registry sender-registry)))

  site-registry agent-registry sender-registry
  (setf site-registry) (setf agent-registry) (setf sender-registry)

  ((send (id msg) :reads (site-registry agent-registry sender-registry))
   (alexandria:if-let ((site (registry-entry site-registry id)))
     (alexandria:when-let ((sender-fn (registry-entry sender-registry site)))
       (funcall sender-fn id msg))
     (alexandria:when-let ((agent (registry-entry agent-registry id)))
       (setf (agent-message agent) msg)
       t))))

