;;;; agent.lisp

(in-package #:cl-multiagent-system)

(define-entity data-table 
    (&rest data-plist &aux (table (alexandria:plist-hash-table data-plist)))
    (:declarations ((type list data-plist) (type hash-table table)))

  ((field (key &optional (default +no-value+)) :reads (table))
   (gethash key table default))
  (((setf field) (value key &optional default) :writes (table)
                 :declarations ((ignore default)))
   (if (no-value-p value)
     (remhash key table)
     (setf (gethash key table) value)))

  ((read-fields (fn) :reads (table))
   (funcall fn table))
  ((write-fields (fn) :writes (table))
   (funcall fn table)))

;;-----------------------------------------------------------------------------

(defparameter *agent-loop-fn-ctor* 
  (constantly nil))
(defparameter *agent-start-fn-ctor* 
  (constantly nil))
(defparameter *agent-stop-fn-ctor* 
  (constantly nil))
(defparameter *agent-data-table-ctor* 
  (lambda (instance-id role-id)
    (declare (ignore instance-id role-id)) 
    (make-data-table)))

(define-entity agent
    (instance-id 
     &key role-id (loop-fn (funcall *agent-loop-fn-ctor* instance-id role-id)) 
          (start-fn (funcall *agent-start-fn-ctor* instance-id role-id)) 
          (stop-fn (funcall *agent-stop-fn-ctor* instance-id role-id))
          (data-table (funcall *agent-data-table-ctor* instance-id role-id))
          host-thread
     &aux (message-queue (make-queue :empty-value +no-value+)) running-p)
    (:declarations
      ((type (or null (function (agent))) loop-fn start-fn stop-fn)
       (type boolean running-p)))

  instance-id role-id loop-fn start-fn stop-fn data-table host-thread running-p
  (setf loop-fn) (setf start-fn) (setf stop-fn) (setf data-table)
  (((setf host-thread) (value) :writes (host-thread) :reads (running-p))
   (if running-p
     host-thread
     (setf host-thread value)))

  ((start () :reads (host-thread))
   (when host-thread
     (funcall host-thread :start-agent self)))
  ((stop () :reads (host-thread))
   (when host-thread
     (funcall host-thread :stop-agent self)))

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
               (dolist (agent (funcall self :get-started-agents))
                 (funcall 
                   self :add-thread
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
                   (funcall self :stop-agent agent))))
             (gt:thread-yield)
             (dolist (agent (funcall self :get-stopped-agents))
               (funcall self :del-thread agent))
             (unless running-p
               (return)))))))

  ((map-agents (fn) :read (agent-threads))
   (alexandria:maphash-keys fn agent-threads))

  ;; private interface
  (((setf start-agent) (agent) :reads (agent-threads) :visibility :private)
   (unless (gethash agent agent-threads)
     (funcall agent :running-p t)
     (queue-push start-queue agent)
     t))
  (((setf stop-agent) (agent) :reads (agent-threads) :visibility :private)
   (when (gethash agent agent-threads)
     (funcall agent :running-p nil)
     (queue-push stop-queue agent)
     t))

  ((get-started-agents () :visibility :private)
   (queue-pop-all start-queue))
  ((get-stopped-agents () :visibility :private)
   (queue-pop-all stop-queue))

  (((setf add-thread) (thread agent) :writes (agent-threads) 
                      :visibility :private)
   (unless (gethash agent agent-threads)
     (setf (gethash agent agent-threads) thread)
     t))
  (((setf del-thread) (agent) :writes (agent-threads) :visibility :private)
   (remhash agent agent-threads)))

