;;;; define-accessor.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-synchronized-entity)

(defparameter *accessor-name-fn*
  (lambda (entity-type resource) 
    (alexandria:symbolicate entity-type "-" resource)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-list-parameters (requiredp optionalp restp keyp)
    (append requiredp 
            (mapcar #'car optionalp) (remove nil (mapcar #'caddr optionalp))
            (when restp `(,restp)) 
            (mapcar #'cadar keyp) (remove nil (mapcar #'caddr keyp))))

  (defun argument-collection-code (args requiredp optionalp restp keyp)
    (alexandria:with-gensyms (arg-coll-code-block opt-value opt-present 
                              opt-presence-var-used key key-value key-present
                              key-presence-var-used)
      `((block ,arg-coll-code-block
          (loop 
            :for ,opt-value :in `(,,@(mapcar #'car optionalp))
            :for ,opt-present :in `(,,@(mapcar #'caddr optionalp))
            :for ,opt-presence-var-used :in 
            `(,,@(mapcar (lambda (s) (not (null (caddr s)))) optionalp))
            :do (if (and ,opt-presence-var-used (not ,opt-present))
                  (return-from ,arg-coll-code-block)
                  (push ,opt-value ,args)))
          ,(if restp
             `(setf ,args (nconc (nreverse ,restp) ,args))
             `(loop :for ,key :in `(,,@(mapcar #'caar keyp))
                    :for ,key-value :in `(,,@(mapcar #'cadar keyp))
                    :for ,key-present :in `(,,@(mapcar #'caddr keyp))
                    :for ,key-presence-var-used :in 
                    `(,,@(mapcar (lambda (s) (not (null (caddr s)))) keyp))
                    :do (unless (and ,key-presence-var-used (not ,key-present))
                          (push ,key ,args)
                          (push ,key-value ,args)))))
        (setf ,args (nconc (list ,@requiredp) (nreverse ,args))))))

  (defun define-getter (entity-type key lambda-list no-value)
    (multiple-value-bind (requiredp optionalp restp keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list)
      (let ((parameters (lambda-list-parameters 
                          requiredp optionalp restp keyp))
            (args (gensym)))
        `(defmacro ,(funcall *accessor-name-fn* entity-type key) 
           (,entity-type ,@lambda-list)
           (alexandria:once-only (,entity-type ,@parameters)
             (declare (ignorable ,@parameters))
             (let (,args)
               ,@(argument-collection-code 
                   args requiredp optionalp restp keyp)
               `(funcall ,,`,entity-type ,,key ,,`',no-value ,@,`,args)))))))

(defun define-setter (entity-type key lambda-list)
  (multiple-value-bind (requiredp optionalp restp keyp)
    (alexandria:parse-ordinary-lambda-list lambda-list)
    (let ((parameters (lambda-list-parameters 
                        requiredp optionalp restp keyp))
          (args (gensym)) (store-var (gensym)))
      `(defsetf ,(funcall *accessor-name-fn* entity-type key) 
           (,entity-type ,@lambda-list) (,store-var)
         (declare (ignorable ,@parameters))
         (let (,args)
           ,@(argument-collection-code 
               args requiredp optionalp restp keyp)
           `(funcall ,,`,entity-type ,,key ,,`,store-var ,@,`,args))))))

(defun define-accessor (entity-type accessor no-value)
  (when (eq (accessor-visibility accessor) :public)
    (let ((key (alexandria:make-keyword (accessor-name accessor)))
          (lambda-list (accessor-lambda-list accessor))) 
      (case (accessor-type accessor)
        (:getter (define-getter entity-type key lambda-list no-value))
        (:setter (define-setter entity-type key lambda-list)))))))

