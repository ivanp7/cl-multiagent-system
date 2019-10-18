;;;; messenger.lisp

(in-package #:cl-multiagent-system)

(define-entity messenger
    (&key (site-registry (make-registry)) (agent-registry (make-registry)) 
     &aux (table (make-hash-table :test 'equal)))
    (:declarations
      ((type registry site-registry agent-registry)
       (type hash-table table)))

  site-registry agent-registry

  ((site-known-p (site) :reads (table))
   (if site
     (nth-value 1 (gethash site table))
     t))
  ((map-sites (fn) :reads (table))
   (funcall fn nil)
   (alexandria:maphash-keys fn table)
   t)

  ((add-site (site sender-fn) :writes (table) :calls (site-known-p))
   (when (and sender-fn (not (site-known-p site)))
     (setf (gethash site table) sender-fn)
     t))
  ((del-site (site) :writes (table))
   (remhash site table))

  ((send (id msg) :reads (site-registry agent-registry table) 
         :calls (site-known-p))
   (alexandria:if-let ((site (registry-entry site-registry id)))
     (alexandria:when-let ((sender-fn (gethash site table)))
       (funcall sender-fn id msg))
     (alexandria:when-let ((agent (registry-entry agent-registry id)))
       (setf (agent-message agent) msg)
       t))))

