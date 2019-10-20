;;;; agent.lisp

(in-package #:cl-multiagent-system)

(defparameter *agent-loop-fn-ctor* 
  (constantly nil))
(defparameter *agent-start-fn-ctor* 
  (constantly nil))
(defparameter *agent-stop-fn-ctor* 
  (constantly nil))
(defparameter *agent-data-ctor* 
  (lambda (instance-id role-id)
    (declare (ignore instance-id role-id)) 
    (make-registry)))

(define-entity agent
    (instance-id 
     &key role-id (loop-fn (funcall *agent-loop-fn-ctor* instance-id role-id)) 
          (start-fn (funcall *agent-start-fn-ctor* instance-id role-id)) 
          (stop-fn (funcall *agent-stop-fn-ctor* instance-id role-id))
          (data (funcall *agent-data-ctor* instance-id role-id))
          host-thread
     &aux (message-queue (make-queue :empty-value +no-value+)) running-p)
    (:declarations
      ((type (or null (function (agent))) loop-fn start-fn stop-fn)
       (type boolean running-p)))

  instance-id role-id loop-fn start-fn stop-fn data host-thread running-p
  (setf loop-fn) (setf start-fn) (setf stop-fn) (setf data)
  (((setf host-thread) (value) :writes (host-thread) :reads (running-p))
   (if running-p
     host-thread
     (setf host-thread value)))

  ((start () :reads (host-thread))
   (when host-thread
     (entity-accessor :start-agent host-thread self)))
  ((stop () :reads (host-thread))
   (when host-thread
     (entity-accessor :stop-agent host-thread self)))

  ((message (&key keep))  ; no read-write lock, as queue is synchronized
   (if keep
     (queue-front message-queue)
     (queue-pop message-queue)))
  (((setf message) (msg)) ; no read-write lock, as queue is synchronized
   (queue-push message-queue msg))

  ;; private interface
  (((setf running-p) (value) :writes (running-p) :visibility :private)
   (setf running-p value)))

;;-----------------------------------------------------------------------------

(define-thread multiagent-thread
    (&aux (agent-threads (make-hash-table :test 'eq))
          (start-queue (make-queue)) (stop-queue (make-queue)))
    (:declarations
      ((type hash-table agent-threads)
       (type queue start-queue stop-queue))
     :body
      ((gt:with-green-thread
         (loop
           (let ((running-p (multiagent-thread-running-p self))) 
             (if running-p
               (dolist (agent (entity-accessor :get-started-agents self))
                 (entity-accessor 
                   :add-thread self
                   (gt:with-green-thread
                     (alexandria:when-let ((start-fn (agent-start-fn agent)))
                       (funcall start-fn agent))
                     (loop
                       (if (agent-running-p agent)
                         (alexandria:when-let ((loop-fn (agent-loop-fn agent)))
                           (funcall loop-fn agent))
                         (return))
                       (gt:thread-yield))
                     (alexandria:when-let ((stop-fn (agent-stop-fn agent)))
                       (funcall stop-fn agent)))
                   agent))
               (multiagent-thread-map-agents
                 self
                 (lambda (agent)
                   (entity-accessor :stop-agent self agent))))
             (gt:thread-yield)
             (dolist (agent (entity-accessor :get-stopped-agents self))
               (entity-accessor :del-thread self agent))
             (unless running-p
               (return)))))))

  ((number-of-agents () :reads (agent-threads))
   (hash-table-count agent-threads))
  ((map-agents (fn) :reads (agent-threads))
   (alexandria:maphash-keys fn agent-threads)
   t)

  ;; private interface
  ((start-agent (agent) :reads (agent-threads) :visibility :private)
   (unless (gethash agent agent-threads)
     (setf (entity-accessor :running-p agent) t)
     (queue-push start-queue agent)
     t))
  ((stop-agent (agent) :reads (agent-threads) :visibility :private)
   (when (gethash agent agent-threads)
     (setf (entity-accessor :running-p agent) nil)
     (queue-push stop-queue agent)
     t))

  ((get-started-agents () :visibility :private)
   (queue-pop-all start-queue))
  ((get-stopped-agents () :visibility :private)
   (queue-pop-all stop-queue))

  ((add-thread (thread agent) :writes (agent-threads) 
                      :visibility :private)
   (unless (gethash agent agent-threads)
     (setf (gethash agent agent-threads) thread)
     t))
  ((del-thread (agent) :writes (agent-threads) :visibility :private)
   (remhash agent agent-threads)))

