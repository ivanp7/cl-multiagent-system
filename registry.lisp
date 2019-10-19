;;;; registry.lisp

(in-package #:cl-multiagent-system)

(define-entity registry
    (&optional default-value (value-test #'equal) 
     &aux (table (make-hash-table :test 'equal)))
    (:declarations
      ((type hash-table table)))

  default-value (setf default-value) value-test (setf value-test)

  ((entry (key &optional (default +no-value+)) :reads (default-value table))
   (let ((default (if (no-value-p default) default-value default)))
     (gethash key table default)))
  (((setf entry) (value key &optional default) :writes (table)
                 :declarations ((ignore default)))
   (if (funcall value-test value default-value)
     (remhash key table)
     (setf (gethash key table) value)))

  ((keys () :reads (table))
   (alexandria:hash-table-keys table))
  ((entry-present-p (key) :reads (table))
   (nth-value 1 (gethash key table)))
  
  ((map-entries (fn) :reads (table))
   (maphash fn table)
   t)

  ((add-entry (key value) :writes (table) :calls (entry-present-p))
   (unless (entry-present-p key)
     (setf (gethash key table) value)
     t))
  ((del-entry (key) :writes (table))
   (remhash key table)))

