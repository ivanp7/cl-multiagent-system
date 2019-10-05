;;;; thread.lisp

(in-package #:cl-multiagent-system)

(defmacro define-thread (thread-type lambda-list 
                         (&key declarations initialization body) 
                         &rest accessors)
  (setf accessors (prepare-accessors thread-type accessors))
  (alexandria:with-gensyms (thread-fn running-p thread)
    (let ((lambda-list (append-aux-to-lambda-list 
                         lambda-list running-p thread))
          (declarations (append 
                          declarations
                          `((type boolean ,running-p)
                            (type (or null bt:thread) ,thread))))
          (accessors (append 
                       accessors
                       `(((running-p () :reads (,running-p))
                          ,running-p)
                         ((start () :writes (,running-p))
                          (unless (and ,thread (bt:thread-alive-p ,thread))
                            (setf ,thread 
                                  (bt:make-thread 
                                    (lambda () (funcall ,thread-fn self))) 
                                  ,running-p t)))
                         ((stop () :writes (,running-p))
                          (when ,running-p
                            (setf ,running-p nil)
                            t))
                         ((destroy () :writes (,running-p))
                          (when (and ,thread (bt:thread-alive-p ,thread))
                            (bt:destroy-thread ,thread)
                            (setf ,thread nil ,running-p nil)
                            t))))))
      `(progn
         ,(type-definition thread-type)
         ,@(accessors-definitions thread-type accessors)
         (let ((,thread-fn (lambda (self)
                             (declare (ignorable self))
                             ,@body))) 
           ,(constructor-definition thread-type lambda-list 
                                    declarations initialization accessors))))))

