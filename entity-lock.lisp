;;;; entity-lock.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

;;; https://en.wikipedia.org/wiki/Readers-writers_problem

(in-package #:cl-multiagent-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-read-write-lock-vars ()
    (list (gensym "READ-COUNT") (gensym "COUNT-ACCESS") 
          (gensym "RESOURCE-ACCESS") (gensym "SERVICE-QUEUE")))

  (defun rw-lock-read-count (lock-vars)
    (first lock-vars))

  (defun rw-lock-read-count-access (lock-vars)
    (second lock-vars))

  (defun rw-lock-resource-access (lock-vars)
    (third lock-vars))

  (defun rw-lock-service-queue (lock-vars)
    (fourth lock-vars))

  (defun read-write-lock-bindings (lock-vars)
    `((,(rw-lock-read-count lock-vars) 0)
      (,(rw-lock-read-count-access lock-vars) 
        #1=(bt:make-semaphore :count 1))
      (,(rw-lock-resource-access lock-vars) #1#)
      (,(rw-lock-service-queue lock-vars) #1#)))

  (defun read-write-lock-declarations (lock-vars)
    `((ignorable ,(rw-lock-read-count lock-vars) 
                 ,(rw-lock-read-count-access lock-vars)))))

(defmacro with-read-write-lock-held ((write-locks read-locks) &body body)
  (flet ((writer-entry (lock-vars)
           `((bt:wait-on-semaphore ,(rw-lock-service-queue lock-vars))
             (bt:wait-on-semaphore ,(rw-lock-resource-access lock-vars))
             (bt:signal-semaphore ,(rw-lock-service-queue lock-vars))))
         (writer-exit (lock-vars)
           `((bt:signal-semaphore ,(rw-lock-resource-access lock-vars))))
         (reader-entry (lock-vars)
           `((bt:wait-on-semaphore ,(rw-lock-service-queue lock-vars))
             (bt:wait-on-semaphore ,(rw-lock-read-count-access lock-vars))
             (when (zerop ,(rw-lock-read-count lock-vars))
               (bt:wait-on-semaphore ,(rw-lock-resource-access lock-vars)))
             (incf ,(rw-lock-read-count lock-vars))
             (bt:signal-semaphore ,(rw-lock-service-queue lock-vars))
             (bt:signal-semaphore ,(rw-lock-read-count-access lock-vars))))
         (reader-exit (lock-vars)
           `((bt:wait-on-semaphore ,(rw-lock-read-count-access lock-vars))
             (decf ,(rw-lock-read-count lock-vars))
             (when (zerop ,(rw-lock-read-count lock-vars))
               (bt:signal-semaphore ,(rw-lock-resource-access lock-vars)))
             (bt:signal-semaphore ,(rw-lock-read-count-access lock-vars)))))
    `(progn
       ,@(loop :for write-lock :in write-locks 
               :append (writer-entry write-lock))
       ,@(loop :for read-lock :in read-locks 
               :append (reader-entry read-lock))
       ,@body
       ,@(loop :for write-lock :in write-locks 
               :append (writer-exit write-lock))
       ,@(loop :for read-lock :in read-locks 
               :append (reader-exit read-lock)))))

