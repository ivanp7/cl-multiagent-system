# cl-enhanced-structures                                           

*cl-enhanced-structures* is a more flexible kind of structures.    

## Usage                                                           

An enhanced structure is defined with the macro:         

```lisp                                                            
(define-structure type-name (&key parameters body-macros bindings init-forms 
                                  getters setters post-forms))
```                                                                

The easiest way to explain how *cl-enhanced-structures* works is
to show what does `define-structure` form expand to.
Let's consider the following example:

```lisp
(define-structure fibonacci-pair
  (:parameters (&optional (f1 1) (f2 f1))
   :body-macros ((norm (x y)
                   `(when (> ,x ,y)
                      (setf ,x ,y))))
   :bindings ((n 1) sum)
   :init-forms ((norm f1 f2))
   :getters (n ; short getter form
             (first () ; full getter form
               f1)
             (second ()
               f2)
             sum
             (proceed (&optional (times 1))
               (dotimes (i times)
                 (setf n (1+ n)
                       f1 f2
                       f2 sum
                       sum (+ f1 f2)))))
    :setters ((first () ; full setter form, short form is allowed too
                (setf f1 value) ; 'value' is a value given in a setf form 
                (norm f1 f2)
                value)
              (second ()
                (setf f2 value)
                (norm f1 f2)
                value))
    :post-forms ((setf sum (+ f1 f2)))))
```

This generates a type definition

```lisp
(deftype fibonacci-pair () '(function (symbol &optional * &rest *) *))
```

an instance constructor function

```lisp
(defun make-fibonacci-pair (&optional (f1 1) (f2 f1))
  (let ((lock (bordeaux-threads:make-recursive-lock))
        self) ; reference to an instance itself, can be used in user code
    (declare (ignorable self))
    (macrolet ((norm (x y)
                 `(when (> ,x ,y)
                    (setf ,x ,y))))
      (let* ((n 1) sum) ; bindings
        (when (> f1 f2) (rotatef f1 f2)) ; init-form
        (let ((#:obj616
               (lambda (#:key617 &optional (value no-value) &rest args)
                 (declare (ignorable value args))
                 ;; access is synchronized
                 (bordeaux-threads:with-recursive-lock-held (lock)
                   (if (or (eq value '#:missing-value720)
                           (eq value no-value))
                     (ecase #:key617 ; getters
                       (:n n)
                       (:first (destructuring-bind () args f1))
                       (:second (destructuring-bind () args f2))
                       (:sum sum)
                       (:proceed
                         (destructuring-bind (&optional (times 1)) args
                           (dotimes (i times)
                             (setf n (1+ n)
                                   f1 f2
                                   f2 sum
                                   sum (+ f1 f2)))))
                       (:acquire-lock
                         (bt:acquire-recursive-lock lock)
                         t)
                       (:release-lock
                         (bt:release-recursive-lock lock)
                         t))
                     (ecase #:key617 ; setters
                       (:first
                         (destructuring-bind () args
                           (setf f1 value)
                           (norm f1 f2)
                           value))
                       (:second
                         (destructuring-bind () args
                           (setf f2 value)
                           (norm f1 f2)
                           value))
                       (#:self-key619
                        (setf self value))))))))
          (funcall #:obj616 '#:self-key619 #:obj616) ; setting 'self' variable
          (setf sum (+ f1 f2)) ; post-form
          (the fibonacci-pair #:obj616))))))
```

getter macros

```lisp
(defmacro fibonacci-pair-n (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:n ,''#:missing-value720
              ,@args)))

(defmacro fibonacci-pair-first (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:first ,''#:missing-value720
              ,@args)))

(defmacro fibonacci-pair-second (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:second ,''#:missing-value720
              ,@args)))

(defmacro fibonacci-pair-sum (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:sum ,''#:missing-value720
              ,@args)))

(defmacro fibonacci-pair-proceed (fibonacci-pair &optional (times 1))
  (declare (ignorable times))
  (let ((args
          ;; macro lambda list parsing code
          (let ((#:g661 (list)) #:g662)
            (block #:g660
                   (loop :for opt-value :in `(,times)
                         :for opt-present :in `(,nil)
                         :for opt-presence-var-used :in `(,nil)
                         :do (if (and opt-presence-var-used
                                      (not opt-present))
                               (return-from #:g660)
                               (push opt-value #:g662)))
                   (loop :for key :in `nil
                         :for key-value :in `nil
                         :for key-present :in `nil
                         :for key-presence-var-used :in `nil
                         :do (unless
                               (and key-presence-var-used
                                    (not key-present))
                               (push key-value #:g662)
                               (push key #:g662))))
            (nconc #:g661 (nreverse #:g662)))))
    `(funcall ,fibonacci-pair ,:proceed ,''#:missing-value720
              ,@args)))

(defmacro fibonacci-pair-acquire-lock (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:acquire-lock ,''#:missing-value720
              ,@args)))

(defmacro fibonacci-pair-release-lock (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:release-lock ,''#:missing-value720
              ,@args)))
  
```

setter macros

```lisp
(defsetf fibonacci-pair-first (fibonacci-pair) (new-value)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:first ,new-value
              ,@args)))

(defsetf fibonacci-pair-second (fibonacci-pair) (new-value)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:second ,new-value
              ,@args))))
```

and a lock control wrapper

```lisp
(defmacro with-fibonacci-pair-lock-held ((fibonacci-pair) &body body)
 `(progn
     (funcall ,fibonacci-pair :acquire-lock)
     ,@enhanced-structures::body
     (funcall ,fibonacci-pair :release-lock)))
```

## Author                                                          

Ivan Podmazov (ivanpzv8@gmail.com)                                 

## [License](LICENSE)                                              

Copyright (c) 2019 Ivan Podmazov                                   

