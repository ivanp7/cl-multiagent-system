;;;; entity-utils.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-multiagent-system)

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

  (defmacro accessor-descriptor-plist (accessor)
    `(cddar ,accessor))

  (defun accessor-written-resources (accessor)
    (getf (accessor-descriptor-plist accessor) :writes))
  (defun (setf accessor-written-resources) (value accessor)
    (setf (getf (accessor-descriptor-plist accessor) :writes) value))

  (defun accessor-read-resources (accessor)
    (getf (accessor-descriptor-plist accessor) :reads))
  (defun (setf accessor-read-resources) (value accessor)
    (setf (getf (accessor-descriptor-plist accessor) :reads) value))

  (defun accessor-write-locks (accessor)
    (getf (accessor-descriptor-plist accessor) :writes))
  (defun (setf accessor-write-locks) (value accessor)
    (setf (getf (accessor-descriptor-plist accessor) :writes) value))

  (defun accessor-read-locks (accessor)
    (getf (accessor-descriptor-plist accessor) :reads))
  (defun (setf accessor-read-locks) (value accessor)
    (setf (getf (accessor-descriptor-plist accessor) :reads) value))

  (defun accessor-called-accessors (accessor)
    (getf (accessor-descriptor-plist accessor) :calls))
  (defun (setf accessor-called-accessors) (value accessor)
    (setf (getf (accessor-descriptor-plist accessor) :calls) value))

  (defun accessor-visibility (accessor)
    (ecase (getf (accessor-descriptor-plist accessor) :visibility :public)
      (:public :public)
      (:private :private)))

  (defun accessor-declarations (accessor)
    (getf (accessor-descriptor-plist accessor) :declarations))

  (defun accessor-body (accessor)
    (cdr accessor)))

