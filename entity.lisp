;;;; entity.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-multiagent-system)

(defparameter *constructor-name-fn*
  (lambda (entity-type) 
    (alexandria:symbolicate "MAKE-" entity-type)))

(defmacro make-constructor-name (entity-type)
  `(funcall *constructor-name-fn* ,entity-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun determine-all-used-resources (accessors)
    (macrolet ((rlist-of (rw) `(car ,rw))
               (wlist-of (rw) `(cdr ,rw)))
      (labels ((process (accessor rw-list visited-list)
                 (unless (find (accessor-full-name accessor) visited-list
                               :test #'equal)
                   (push (accessor-full-name accessor) visited-list)
                   (setf (wlist-of rw-list)
                         (union (wlist-of rw-list)
                                (accessor-written-resources accessor)
                                :test #'equal)
                         (rlist-of rw-list)
                         (union (rlist-of rw-list)
                                (accessor-read-resources accessor)
                                :test #'equal))
                   (dolist (callee (accessor-called-accessors accessor))
                     (alexandria:if-let 
                         ((called-accessor (find callee accessors
                                                 :key #'accessor-full-name
                                                 :test #'equal)))
                       (process called-accessor rw-list visited-list)
                       (error "No such accessor ~A declared as called."
                              callee))))))
        (dolist (accessor accessors)
          (let ((rw-list (cons () ())))
            (process accessor rw-list ())
            (setf (accessor-written-resources accessor) (wlist-of rw-list)
                  (accessor-read-resources accessor) (rlist-of rw-list)
                  (accessor-called-accessors accessor) ())))))
    accessors)

  (defun replace-resources-with-locks (accessors)
    (let* ((written-resources-lists 
             (mapcar #'accessor-written-resources accessors)) 
           (all-written-resources 
             (reduce (lambda (s1 s2) (union s1 s2 :test #'equal))
                     written-resources-lists :initial-value ()))
           (groups-map (make-hash-table :test 'equal)) 
           (rw-locks-map (make-hash-table :test 'equal)))
      (dolist (res all-written-resources)
        (push res 
              (gethash (reduce 
                         (lambda (s1 s2) (intersection s1 s2 :test #'equal))
                         (remove-if-not (lambda (lst) 
                                          (member res lst :test #'equal))
                                        written-resources-lists))
                       groups-map)))
      (alexandria:maphash-values (lambda (group)
                                   (setf (gethash group rw-locks-map)
                                         (make-read-write-lock-vars)))
                                 groups-map)
      (dolist (accessor accessors)
        (let (write-locks read-locks)
          (maphash 
            (lambda (group rw-lock)
              (cond
                ((intersection (accessor-written-resources accessor) group
                               :test #'equal)
                 (push rw-lock write-locks))
                ((intersection (accessor-read-resources accessor) group
                               :test #'equal)
                 (push rw-lock read-locks))))
            rw-locks-map)
          (setf (accessor-write-locks accessor) write-locks
                (accessor-read-locks accessor) read-locks)))
      (alexandria:hash-table-values rw-locks-map)))

  (defun labels-declarations (accessors)
    (mapcar (lambda (accessor)
              `(,(accessor-full-name accessor)
                (,@(accessor-full-lambda-list accessor))
                (declare ,@(accessor-declarations accessor))
                (let ((self self))
                  (declare (ignorable self))
                  ,@(accessor-body accessor))))
            accessors))

  (defun construct-case (accessor args)
    (let ((key (alexandria:make-keyword (accessor-name accessor)))) 
      `(,key
         (with-read-write-lock-held (,(accessor-write-locks accessor) 
                                     ,(accessor-read-locks accessor))
           (apply #',(accessor-full-name accessor) ,args)))))

  (defun define-constructor (entity-type lambda-list declarations
                             initialization accessors)
    (determine-all-used-resources accessors)
    (let ((rw-locks-vars (replace-resources-with-locks accessors))
          (getters (remove-if-not 
                     (lambda (accessor) 
                       (eq (accessor-type accessor) :getter)) accessors))
          (setters (remove-if-not 
                     (lambda (accessor) 
                       (eq (accessor-type accessor) :setter)) accessors)))
      (alexandria:with-gensyms (key value args)
        `(defun ,(make-constructor-name entity-type) (,@lambda-list)
           (declare ,@declarations)
           (let (self)
             (labels (,@(labels-declarations accessors))
               (let (,@(loop :for rwlock-vars :in rw-locks-vars
                             :append (read-write-lock-bindings rwlock-vars)))
                 (declare ,@(loop :for rwlock-vars :in rw-locks-vars
                                  :append (read-write-lock-declarations 
                                            rwlock-vars)))
                 (setf self
                       (lambda (,key ,value &rest ,args)
                         (declare (type symbol ,key))
                         (if (no-value-p ,value)
                           (ecase ,key
                             ,@(mapcar (lambda (getter) 
                                         (construct-case getter args))
                                       getters))
                           (progn
                             (push ,value ,args)
                             (ecase ,key
                               ,@(mapcar (lambda (setter) 
                                           (construct-case setter args))
                                         setters)))))))
               ,@initialization)
             (the ,entity-type self)))))))

(deftype synchronized-entity () '(function (symbol * &rest *) *))

(deftype missing-value () 'symbol)
(alexandria:define-constant +no-value+ '#.(gensym "NO-VALUE") 
                            :test (constantly t))

(defmacro no-value-p (value)
  `(eq ,value +no-value+))

(defmacro define-synchronized-entity (entity-type lambda-list 
                                      (&key declarations initialization) 
                                      &rest accessors)
  (setf accessors (mapcar #'normalize-accessor-description accessors))
  (when (/= (length accessors) 
            (length (remove-duplicates accessors :test #'equal 
                                       :key #'accessor-full-name)))
    (error "Multiple accessor definitions provided."))
  `(progn
     (deftype ,entity-type () 'synchronized-entity)
     ,@(mapcar (lambda (accessor) 
                 (define-accessor entity-type accessor))
               accessors)
     ,(define-constructor entity-type lambda-list declarations initialization 
                          accessors)))

;;;----------------------------------------------------------------------------

(defun append-aux-to-lambda-list (lambda-list &rest specifiers)
  (append lambda-list 
          (unless (member '&aux lambda-list :test #'eq)
            `(&aux))
          specifiers))

