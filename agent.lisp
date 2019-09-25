;;;; agent.lisp

(in-package #:cl-multiagent-system)

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
   (funcall fn data))

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
     `(,,`',(funcall *accessor-name-fn* 'agent 'datum) 
        ,agent ,,datum-key ,,@(when def-p `(,default)))))

;;;----------------------------------------------------------------------------

(define-agent-datum-accessor agent-route-fn :route-fn nil)

(defmacro agent-forward-message (agent message)
  (alexandria:once-only (agent message)
    `(alexandria:when-let* ((route-fn (agent-route-fn ,agent)) 
                            (next (funcall route-fn ,agent ,message)))
       (setf (agent-message next) ,message))))

;;;----------------------------------------------------------------------------

(define-synchronized-entity message (&key addresser addressee data-plist
                                     &aux (data (alexandria:plist-hash-table
                                                  data-plist)))
                            (:declarations
                              ((type (or null agent) addresser addressee)
                               (type list data-plist)
                               (type hash-table data)))

  addresser addressee (setf addresser) (setf addressee)
  ((swap-addresses () :writes (addresser addressee))
   (rotatef addresser addressee)
   t)

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

(defmacro define-message-datum-accessor (name datum-key 
                                         &optional (default '+no-value+ def-p))
  `(defmacro ,name (message)
     `(,,`',(funcall *accessor-name-fn* 'message 'datum) 
        ,message ,,datum-key ,,@(when def-p `(,default)))))

