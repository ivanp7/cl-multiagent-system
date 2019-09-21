;;;; accessor-description.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-synchronized-entity)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-accessor-description (accessor)
    (cond
      ((symbolp accessor)
       `((,accessor () :reads (,accessor))
         ,accessor))
      ((and (listp accessor) (eq (car accessor) 'setf) 
            (symbolp (cadr accessor)) (null (cddr accessor)))
       `((,accessor (value) :writes (,(cadr accessor)))
         (setf ,(cadr accessor) value)))
      (t accessor)))

  (defun accessor-full-name (accessor)
    (caar accessor))

  (defun accessor-type (accessor)
    (let ((name (accessor-full-name accessor))) 
      (cond
        ((symbolp name) 
         :getter)
        ((and (listp name) (eq (car name) 'setf) (symbolp (cadr name)) 
              (null (cddr name)))
         :setter)
        (t (error "Invalid name (~A) in accessor description." name)))))

  (defun accessor-name (accessor)
    (let ((full-name (accessor-full-name accessor))) 
      (case (accessor-type accessor)
        (:getter full-name)
        (:setter (cadr full-name)))))

  (defun accessor-full-lambda-list (accessor)
    (cadar accessor))

  (defun accessor-lambda-list (accessor)
    (let ((full-lambda-list (accessor-full-lambda-list accessor)))
      (case (accessor-type accessor)
        (:getter full-lambda-list)
        (:setter (cdr full-lambda-list)))))

  (defun accessor-store-variable (accessor)
    (case (accessor-type accessor)
      (:getter (error "Getters don't use store variable."))
      (:setter (alexandria:if-let ((store-var (caadar accessor)))
                 store-var
                 (error "No store variable provided for accessor ~A."
                        accessor)))))

  (defun accessor-written-resources (accessor)
    (getf (cddar accessor) :writes))

  (defun (setf accessor-write-locks) (list accessor)
    (setf (getf (cddar accessor) :writes) list))

  (defun accessor-write-locks (accessor)
    (getf (cddar accessor) :writes))

  (defun accessor-read-resources (accessor)
    (getf (cddar accessor) :reads))

  (defun (setf accessor-read-locks) (list accessor)
    (setf (getf (cddar accessor) :reads) list))

  (defun accessor-read-locks (accessor)
    (getf (cddar accessor) :reads))

  (defun accessor-visibility (accessor)
    (getf (cddar accessor) :visibility :public))

  (defun accessor-body (accessor)
    (cdr accessor)))

