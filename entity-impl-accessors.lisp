;;;; entity-impl-accessors.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-multiagent-system)

(defparameter *accessor-name-fn*
  (lambda (entity-type resource) 
    (alexandria:symbolicate entity-type "-" resource)))

(defmacro make-accessor-name (entity-type resource)
  `(funcall *accessor-name-fn* ,entity-type ,resource))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun patch-lambda-list-for-macro (lambda-list) 
    (multiple-value-bind (requiredp optionalp restp keyp allow-other-keys-p)
        (alexandria:parse-ordinary-lambda-list lambda-list :normalize nil)
      (let ((lambda-list (nreverse requiredp))
            (optionalp (mapcar (lambda (opt)
                                 (if (and (listp opt) (= 1 (length opt)))
                                   (first opt) 
                                   opt))
                               optionalp))
            (keyp (mapcar (lambda (key)
                            (if (and (listp key) (= 1 (length key)))
                              (first key) 
                              key))
                          keyp))) 
        (when optionalp
          (push '&optional lambda-list)
          (dolist (opt optionalp)
            (when (and (listp opt) (> (length opt) 1))
              (setf (second opt) `',(second opt))))
          (setf lambda-list (nconc (nreverse optionalp) lambda-list)))
        (when restp
          (push '&rest lambda-list)
          (push restp lambda-list))
        (when keyp
          (push '&key lambda-list)
          (dolist (key keyp)
            (when (and (listp key) (> (length key) 1))
              (setf (second key) `',(second key))))
          (setf lambda-list (nconc (nreverse keyp) lambda-list)))
        (when allow-other-keys-p
          (push '&allow-other-keys lambda-list))
        (nreverse lambda-list))))

  (defun lambda-list-parameters (requiredp optionalp restp keyp)
    (append requiredp 
            (mapcar #'car optionalp) (remove nil (mapcar #'caddr optionalp))
            (when restp `(,restp)) 
            (mapcar #'cadar keyp) (remove nil (mapcar #'caddr keyp))))

  (defun argument-collection-code (args requiredp optionalp restp keyp)
    (alexandria:with-gensyms (arguments-collection-code opt-value opt-present 
                              opt-presence-var-used key key-value key-present
                              key-presence-var-used)
      `((block ,arguments-collection-code
          (loop 
            :for ,opt-value :in `(,,@(mapcar #'car optionalp))
            :for ,opt-present :in `(,,@(mapcar #'caddr optionalp))
            :for ,opt-presence-var-used :in 
            `(,,@(mapcar (lambda (s) (not (null (caddr s)))) optionalp))
            :do (if (and ,opt-presence-var-used (not ,opt-present))
                  (return-from ,arguments-collection-code)
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

  (defun getter-definition (entity-type key lambda-list)
    (multiple-value-bind (requiredp optionalp restp keyp)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (let ((name (make-accessor-name entity-type key)) 
            (lambda-list (patch-lambda-list-for-macro lambda-list))
            (parameters (lambda-list-parameters 
                          requiredp optionalp restp keyp))
            (args (gensym)))
        `(defmacro ,name (,entity-type ,@lambda-list)
           (alexandria:once-only (,entity-type ,@parameters)
             (declare (ignorable ,@parameters))
             (let (,args)
               ,@(argument-collection-code 
                   args requiredp optionalp restp keyp)
               `(funcall ,,`,entity-type ,,key +no-value+ ,@,`,args)))))))

(defun setter-definition (entity-type key lambda-list)
  (multiple-value-bind (requiredp optionalp restp keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let ((name (make-accessor-name entity-type key)) 
          (lambda-list (patch-lambda-list-for-macro lambda-list))
          (parameters (lambda-list-parameters 
                        requiredp optionalp restp keyp))
          (args (gensym)) (store-var (gensym)))
      `(defsetf ,name (,entity-type ,@lambda-list) (,store-var)
         (declare (ignorable ,@parameters))
         (let (,args)
           ,@(argument-collection-code 
               args requiredp optionalp restp keyp)
           `(funcall ,,`,entity-type ,,key ,,`,store-var ,@,`,args))))))

(defun accessor-definition (entity-type accessor)
  (when (eq (accessor-visibility accessor) :public)
    (let ((key (alexandria:make-keyword (accessor-name accessor)))
          (lambda-list (accessor-lambda-list accessor))) 
      (case (accessor-type accessor)
        (:getter (getter-definition entity-type key lambda-list))
        (:setter (setter-definition entity-type key lambda-list)))))))

(defun accessors-definitions (entity-type accessors)
  (mapcar (lambda (accessor)
            (accessor-definition entity-type accessor))
          accessors))

