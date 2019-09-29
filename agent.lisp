;;;; agent.lisp

(in-package #:cl-multiagent-system)

(defmacro define-agent (agent-type lambda-list 
                        (&key declarations initialization body)
                        &rest accessors)
  (alexandria:with-gensyms (thread-fn running-p thread)
    `(let ((,thread-fn (lambda (self) 
                         (declare (ignorable self))
                         ,@body)))
       (define-synchronized-entity ,agent-type ,(append-aux-to-lambda-list 
                                                  lambda-list running-p thread)
           (:declarations (,@declarations 
                           (type boolean ,running-p) 
                           (type (or null bt:thread) ,thread))
            :initialization (,@initialization))
  
         ,@accessors
  
         ((running-p () :reads (,running-p))
          ,running-p)
         ((start () :writes (,running-p))
          (unless (and ,thread (bt:thread-alive-p ,thread))
            (setf ,thread (bt:make-thread 
                            (lambda () (funcall ,thread-fn self))) 
                  ,running-p t)))
         ((stop () :writes (,running-p))
          (when ,running-p
            (setf ,running-p nil)
            t))
         ((kill () :writes (,running-p))
          (when (and ,thread (bt:thread-alive-p ,thread))
            (bt:destroy-thread ,thread)
            (setf ,thread nil ,running-p nil)
            t))))))

;;;----------------------------------------------------------------------------

(define-synchronized-entity htable 
    (&rest data-plist &aux (data (alexandria:plist-hash-table data-plist)))
    (:declarations ((type list data-plist) (type hash-table data)))

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

(defparameter *agent-loop-fn-ctor* (constantly nil))
(defparameter *agent-start-fn-ctor* (constantly nil))
(defparameter *agent-stop-fn-ctor* (constantly nil))
(defparameter *agent-route-fn-ctor* (constantly nil))
(defparameter *agent-data-plist-ctor* (constantly ()))

;; dynamic agent
(define-agent dagent
    (type-id instance-id 
     &key (loop-fn (funcall *agent-loop-fn-ctor* type-id)) 
          (start-fn (funcall *agent-start-fn-ctor* type-id)) 
          (stop-fn (funcall *agent-stop-fn-ctor* type-id)) 
          (route-fn (funcall *agent-route-fn-ctor* type-id)) 
          (data-plist (funcall *agent-data-plist-ctor* type-id instance-id))
     &aux (message-queue (make-queue :empty-value +no-value+))
          (data (apply #'make-htable data-plist)))

    (:declarations 
      ((type (or null (function (dagent))) loop-fn start-fn stop-fn)
       (type (or null (function (dagent *) dagent)) route-fn)
       (type list data-plist)
       (type queue message-queue)
       (type htable data))
     :body
      ((alexandria:when-let ((start-fn (dagent-start-fn self)))
         (funcall start-fn self))
       (loop
         (if (dagent-running-p self)
           (alexandria:when-let ((loop-fn (dagent-loop-fn self)))
             (funcall loop-fn self))
           (return))
         (bt:thread-yield))
       (alexandria:when-let ((stop-fn (dagent-stop-fn self)))
         (funcall stop-fn self))))
  
  type-id instance-id loop-fn start-fn stop-fn route-fn
  (setf loop-fn) (setf start-fn) (setf stop-fn) (setf route-fn)
  data

  (((setf message) (msg)) ; no read-write lock, as queue is synchronized
   (queue-push message-queue msg))
  ((message (&key keep))  ; no read-write lock, as queue is synchronized
   (if keep
     (queue-front message-queue)
     (queue-pop message-queue)))
  ((forward-message (msg) :reads (route-fn))
   (when route-fn
     (alexandria:when-let* ((forward-to (funcall route-fn self msg)))
       (setf (dagent-message forward-to) msg)
       forward-to))))

(defmacro dagent-datum (agent key &optional (default '+no-value def-p))
  `(htable-datum (dagent-data ,agent) ,key ,@(when def-p `(,default))))

(defmacro define-dagent-datum-accessor (name key &optional 
                                                 (default '+no-value+ def-p))
  `(defmacro ,name (agent)
     `(dagent-datum ,agent ,,key ,,@(when def-p `(,default)))))

(defmacro dagent-read-data (agent fn)
  `(htable-read-data (dagent-data ,agent) ,fn))

(defmacro dagent-write-data (agent fn)
  `(htable-write-data (dagent-data ,agent) ,fn))

