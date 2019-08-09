;;;; structure.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:enhanced-structures)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun construct-accessor-case (spec default-body-fn)
    (if (listp spec)
      `(,(alexandria:make-keyword (car spec))
        (destructuring-bind (,@(cadr spec)) args
          ,@(cddr spec)))
      `(,(alexandria:make-keyword spec)
        ,(funcall default-body-fn spec))))

  (defun get-accessor-macro-elements (spec type-name)
    (let ((params (when (listp spec) (cadr spec))))
      (multiple-value-bind (required optional rest keys)
          (alexandria:parse-ordinary-lambda-list params)
        (let* ((case-key (alexandria:make-keyword 
                      (if (listp spec) (car spec) spec)))
               (name (alexandria:symbolicate type-name "-" case-key))
               (vars (append required (mapcar #'car optional)
                             (remove nil (mapcar #'caddr optional))
                             (when rest `(,rest)) (mapcar #'cadar keys)
                             (remove nil (mapcar #'caddr keys))))
               (blk (gensym)) (req-args (gensym)) (rest-args (gensym)))
          (values name params case-key vars
                  (when params
                    `(let ((,req-args (list ,@required)) ,rest-args)
                       (block ,blk
                         (loop :for opt-value :in 
                               `(,,@(mapcar #'car optional))
                               :for opt-present :in
                               `(,,@(mapcar #'caddr optional))
                               :for opt-presence-var-used :in
                               `(,,@(mapcar (lambda (s) 
                                              (not (null (caddr s))))
                                            optional))
                               :do
                               (if (and opt-presence-var-used 
                                        (not opt-present))
                                 (return-from ,blk)
                                 (push opt-value ,rest-args)))
                              ,(if rest
                                 `(setf ,rest-args (nconc (nreverse ,rest) 
                                                          ,rest-args))
                                 `(loop :for key :in
                                        `(,,@(mapcar #'caar keys))
                                        :for key-value :in
                                        `(,,@(mapcar #'cadar keys))
                                        :for key-present :in
                                        `(,,@(mapcar #'caddr keys))
                                        :for key-presence-var-used :in
                                        `(,,@(mapcar (lambda (s)
                                                       (not (null (caddr s))))
                                                     keys))
                                        :do
                                        (unless (and key-presence-var-used
                                                     (not key-present))
                                          (push key-value ,rest-args)
                                          (push key ,rest-args)))))
                       (nconc ,req-args (nreverse ,rest-args))))))))))

(defmacro define-structure (type-name (&key parameters bindings
                                       init-forms getters setters post-forms))
  (alexandria:with-gensyms (lock obj key no-value self-key)
    `(progn
       (defun ,(alexandria:symbolicate "MAKE-" type-name) (,@parameters)
         (let ((,lock (bt:make-lock)) self)
           (declare (ignorable self))
           (let* (,@bindings)
             ,@init-forms
             (let ((,obj 
                     (lambda (,key &optional (value ',no-value)
                                   &rest args)
                       (declare (ignorable value args))
                       (bt:with-lock-held (,lock)
                         (if (eq value ',no-value)
                           (ecase ,key
                             ,@(mapcar
                                 (lambda (spec)
                                   (construct-accessor-case 
                                     spec (lambda (var) `,var)))
                                 getters))
                           (ecase ,key
                             ,@(mapcar
                                 (lambda (spec)
                                   (construct-accessor-case
                                     spec (lambda (var) `(setf ,var value))))
                                 setters)
                             (,self-key (setf self value))))))))
               (funcall ,obj ',self-key ,obj)
               ,@post-forms
               ,obj))))
       ,@(mapcar (lambda (spec)
                   (multiple-value-bind (name params key vars arg-code)
                       (get-accessor-macro-elements spec type-name)
                     `(defmacro ,name (,type-name ,@params)
                        (declare (ignorable ,@vars))
                        (let ((args ,arg-code))
                          `(funcall ,,`,type-name ,,key ,,`'',no-value 
                                    ,@args)))))
                 getters)
       ,@(mapcar (lambda (spec)
                   (multiple-value-bind (name params key vars arg-code)
                       (get-accessor-macro-elements spec type-name) 
                     `(defsetf ,name (,type-name ,@params) (new-value)
                        (declare (ignorable ,@vars))
                        (let ((args ,arg-code))
                          `(funcall ,,`,type-name ,,key ,new-value 
                                    ,@args)))))
                 setters))))

