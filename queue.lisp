;;;; queue.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-synchronized-entity)

;;; public interface

(defvar *default-queue-empty-value* (gensym))

(defstruct queue
  (empty-value *default-queue-empty-value*)

  ;;; private interface

  (front-cons () :type list)
  (back-cons () :type list)
  (lock (bt:make-lock) :type bt:lock :read-only t)
  (condition-variable (bt:make-condition-variable) :read-only t))

;;; public interface

(defun queue-empty-p (queue)
  (bt:with-lock-held ((queue-lock queue))
    (null (queue-front-cons queue))))

(defun queue-front (queue)
  (bt:with-lock-held ((queue-lock queue))
    (car (queue-front-cons queue))))

(defun queue-back (queue)
  (bt:with-lock-held ((queue-lock queue))
    (car (queue-back-cons queue))))

(defun queue-push (queue &rest elements)
  (bt:with-lock-held ((queue-lock queue))
    (let ((new-back-cons (last elements)))
      (if (null (queue-front-cons queue))
        (setf (queue-front-cons queue) elements)
        (setf (cdr (queue-back-cons queue)) elements))
      (setf (queue-back-cons queue) new-back-cons))
    (bt:condition-notify (queue-condition-variable queue))
    (if (null (queue-front-cons queue))
      (queue-empty-value queue)
      (values-list (nreverse elements)))))

(defun queue-pop (queue &key (n 1) (wait t))
  (bt:with-lock-held ((queue-lock queue))
    (flet ((popq ()
             (when (and wait (null (queue-front-cons queue)))
               (bt:condition-wait (queue-condition-variable queue)
                                  (queue-lock queue)))
             (if (null (queue-front-cons queue))
               (queue-empty-value queue)
               (prog1 (pop (queue-front-cons queue))
                 (unless (queue-front-cons queue)
                   (setf (queue-back-cons queue) ())))))) 
      (cond
        ((<= n 0) (queue-empty-value queue))
        ((= n 1) (popq))
        (t (values-list (loop :for i :below n :collect (popq))))))))

