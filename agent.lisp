;;;; agent.lisp

(in-package #:cl-multiagent-system)

(define-synchronized-entity table (&rest data-plist
                                   &aux (data (alexandria:plist-hash-table
                                                data-plist)))
                            (:declarations
                              ((type list data-plist) (type hash-table data)))

  ((datum (key &optional (default +no-value+)) :reads (data))
   (gethash key data default))
  (((setf datum) (value key &optional default) :writes (data)
                 :declarations ((ignore default)))
   (if (no-value-p value)
     (remhash key data)
     (setf (gethash key data) value)))
  ((read-data (fn) :reads (data))
   (funcall fn data))
  ((write-data (fn) :writes (data))
   (funcall fn data)))

;;;----------------------------------------------------------------------------

(define-synchronized-entity agent (type-id instance-id loop-fn
                                   &key start-fn stop-fn route-fn data-plist
                                   &aux running-p thread
                                   (message-queue (make-queue 
                                                    :empty-value +no-value+)) 
                                   (table (apply #'make-table data-plist)))
                            (:declarations 
                              ((type (function (agent) *) loop-fn)
                               (type (or null (function (agent) *)) 
                                     start-fn stop-fn)
                               (type (or null (function (agent *) agent))
                                     route-fn)
                               (type list data-plist)
                               (type boolean running-p)
                               (type (or null bt:thread) thread)
                               (type queue message-queue)
                               (type table table)))

  type-id instance-id loop-fn start-fn stop-fn route-fn running-p table
  (setf loop-fn) (setf start-fn) (setf stop-fn) (setf route-fn)

  (((setf message) (msg))
   (queue-push message-queue msg))
  ((message (&key keep))
   (if keep
     (queue-front message-queue)
     (queue-pop message-queue)))
  ((forward-message (msg) :reads (route-fn))
   (when route-fn
     (alexandria:when-let* ((forward-to (funcall route-fn self msg)))
       (setf (agent-message forward-to) msg)
       forward-to)))

  ((start () :writes (running-p) :reads (loop-fn start-fn stop-fn))
   (unless (and thread (bt:thread-alive-p thread))
     (setf thread
           (bt:make-thread
             (lambda ()
               (alexandria:when-let ((start-fn (agent-start-fn self)))
                 (funcall start-fn self))
               (loop
                 (if (agent-running-p self)
                   (funcall (agent-loop-fn self) self)
                   (return))
                 (bt:thread-yield))
               (alexandria:when-let ((stop-fn (agent-stop-fn self)))
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

(defmacro define-agent-datum-accessor (name datum-key 
                                       &optional (default '+no-value+ def-p))
  `(defmacro ,name (agent)
     `(,,`',(funcall *accessor-name-fn* 'table 'datum)
        (,,`',(funcall *accessor-name-fn* 'agent 'table) ,agent)
         ,,datum-key ,,@(when def-p `(,default)))))

