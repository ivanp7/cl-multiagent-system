;;;; registry.lisp

(in-package #:cl-multiagent-system)

(define-entity registry
    (&aux (table (make-hash-table :test 'equal)))
    (:declarations
      ((type hash-table table)))

  ((entry-present-p (key) :reads (table))
   (nth-value 1 (gethash key table)))
  ((entry (key) :reads (table))
   (gethash key table))
  ((map-entries (fn) :reads (table))
   (maphash fn table)
   t)

  ((add-entry (key value) :writes (table) :calls (entry-present-p))
   (unless (entry-present-p key)
     (setf (gethash key table) value)
     t))
  ((del-entry (key) :writes (table))
   (remhash key table)))

