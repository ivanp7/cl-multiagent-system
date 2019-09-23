;;;; agent.lisp

(in-package #:cl-synchronized-entity)

(define-synchronized-entity agent (type-id instance-id loop-fn
                                   &key start-fn stop-fn data-plist
                                   &aux running-p 
                                   (message-queue (make-queue 
                                                    :empty-value +no-value+))
                                   (data (alexandria:plist-hash-table 
                                           data-plist)) thread)
                            (:declarations 
                              ((type (function (agent) *) loop-fn)
                               (type (or null (function (agent) *)) 
                                     start-fn stop-fn)
                               (type list data-plist)
                               (type boolean running-p)
                               (type queue message-queue)
                               (type hash-table data)
                               (type (or null bt:thread) thread)))
  type-id instance-id loop-fn start-fn stop-fn running-p
  (setf loop-fn) (setf start-fn) (setf stop-fn)
  ((message (&key keep))
   (if keep
     (queue-front message-queue)
     (queue-pop message-queue)))
  (((setf message) (msg))
   (queue-push message-queue msg))
  ((data (key &optional default) :reads (data))
   (gethash key data default))
  (((setf data) (value key) :writes (data))
   (if (no-value-p value)
     (remhash key data)
     (setf (gethash key data) value)))
  ((start () :writes (running-p) :reads (loop-fn start-fn stop-fn))
   (unless (and thread (bt:thread-alive-p thread))
     (setf thread
           (bt:make-thread
             (lambda ()
               (alexandria:if-let ((start-fn (agent-start-fn self)))
                 (funcall start-fn self))
               (loop
                 (if (agent-running-p self)
                   (funcall (agent-loop-fn self) self)
                   (return))
                 (bt:thread-yield))
               (alexandria:if-let ((stop-fn (agent-stop-fn self)))
                 (funcall stop-fn self)))) 
           running-p t)))
  ((stop () :writes (running-p))
   (when running-p
     (setf running-p nil)
     t))
  ((kill () :writes (running-p))
   (when (and thread (bt:thread-alive-p thread))
     (bt:destroy-thread thread)
     (setf thread nil running-p nil)
     t)))

(defmacro define-agent-data-accessor (name data-key)
  `(defmacro ,name (agent)
     `(agent-data ,agent ,,data-key)))

