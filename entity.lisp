;;;; entity.lisp
;;
;;;; Copyright (c) 2019 Ivan Podmazov

(in-package #:cl-synchronized-entity)

(defparameter *constructor-name-fn*
  (lambda (entity-type) 
    (alexandria:symbolicate "MAKE-" entity-type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun labels-declarations (accessors)
    (mapcar (lambda (accessor)
              `(,(accessor-full-name accessor)
                 (,@(accessor-full-lambda-list accessor))
                 (let ((self self))
                   (declare (ignorable self))
                   ,@(accessor-body accessor))))
            accessors))

  (defun replace-resources-with-locks (accessors)
    (let* ((written-resources-lists 
             (mapcar #'accessor-written-resources accessors)) 
           (all-written-resources 
             (reduce (lambda (s1 s2) (union s1 s2 :test #'equal))
                     written-resources-lists))
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

  (defun construct-case (accessor args)
    (let ((key (alexandria:make-keyword (accessor-name accessor)))) 
      `(,key
         (with-read-write-lock-held (,(accessor-write-locks accessor) 
                                     ,(accessor-read-locks accessor))
           (apply #',(accessor-full-name accessor) ,args)))))

  (defun define-constructor (entity-type lambda-list declarations
                             initialization accessors)
    (let ((rw-locks-vars (replace-resources-with-locks accessors))
          (getters (remove-if-not 
                     (lambda (accessor) 
                       (eq (accessor-type accessor) :getter)) accessors))
          (setters (remove-if-not 
                     (lambda (accessor) 
                       (eq (accessor-type accessor) :setter)) accessors)))
      (alexandria:with-gensyms (key value args)
        `(defun ,(funcall *constructor-name-fn* entity-type) (,@lambda-list)
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
                         (if (eq ,value +no-value+)
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

(alexandria:define-constant +no-value+ '#.(gensym "NO-VALUE") 
                            :test (constantly t))

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

