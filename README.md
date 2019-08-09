# cl-enhanced-structures                                           

*cl-enhanced-structures* is a more flexible kind of structures.    

## Usage                                                           

An enhanced structure is defined with the macro:         

```lisp                                                            
(define-structure type-name &key parameters bindings               
                                 init-form getters setters 
                                 post-form)
```                                                                

The easiest way to explain how *cl-enhanced-structures* works is
to show what does `define-structure` form expand to.
Let's consider the following example:

```lisp
(define-structure fibonacci-pair
  :parameters (&optional (f1 1) (f2 f1))
  :bindings ((n 1) sum)
  :init-form (when (> f1 f2) (rotatef f1 f2))
  :getters (n ; short getter form
            (first () ; full getter form
              f1)
            (second ()
              f2)
            sum
            (proceed (&optional (times 1))
              (dotimes (i times)
                (setf f1 f2
                      f2 sum
                      sum (+ f1 f2))))
   :setters ((first () ; full setter form, short form is allowed too
               (setf f1 value) ; 'value' is a value given in a setf form 
               (when (> f1 f2)
                 (setf f2 f1))
               value)
             (second ()
               (setf f2 value)
               (when (> f1 f2)
                 (setf f1 f2))
               value))
   :post-form (setf sum (+ f1 f2)))
```

This generates an instance constructor function

```lisp
(defun make-fibonacci-pair (&optional (f1 1) (f2 f1))
  (let ((#:lock615 (bordeaux-threads:make-lock))
        self) ; reference to an instance itself, can be used in user code
    (declare (ignorable self))
    (let* ((n 1) sum) ; bindings
      (when (> f1 f2) (rotatef f1 f2)) ; init-form
      (let ((#:obj616
             (lambda (#:key617 &optional (value '#:no-value646) &rest args)
               (declare (ignorable value args))
               ;; access is synchronized
               (bordeaux-threads:with-lock-held (#:lock615)
                 (if (eq value '#:no-value646)
                   (ecase #:key617 ; getters
                     (:n n)
                     (:first (destructuring-bind () args f1))
                     (:second (destructuring-bind () args f2))
                     (:sum sum)
                     (:proceed
                       (destructuring-bind (&optional (times 1)) args
                         (dotimes (i times)
                           (setf f1 f2
                                 f2 sum
                                 sum (+ f1 f2))))))
                   (ecase #:key617 ; setters
                     (:first
                       (destructuring-bind () args
                         (setf f1 value)
                         (when (> f1 f2) 
                           (setf f2 f1))
                         value))
                     (:second
                       (destructuring-bind () args
                         (setf f2 value)
                         (when (> f1 f2) 
                           (setf f1 f2))
                         value))
                     (#:self-key619
                      (setf self value))))))))
        (funcall #:obj616 '#:self-key619 #:obj616) ; setting 'self' variable
        (setf sum (+ f1 f2)) ; post-form
        #:obj616))))
```

getter macros

```lisp
(defmacro fibonacci-pair-n (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:n ,''#:no-value646
              ,@args)))

(defmacro fibonacci-pair-first (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:first ,''#:no-value646
              ,@args)))

(defmacro fibonacci-pair-second (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:second ,''#:no-value646
              ,@args)))

(defmacro fibonacci-pair-sum (fibonacci-pair)
  (declare (ignorable))
  (let ((args nil))
    `(funcall ,fibonacci-pair ,:sum ,''#:no-value646
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
    `(funcall ,fibonacci-pair ,:proceed ,''#:no-value646
              ,@args)))

```

and setters

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

## Author                                                          

Ivan Podmazov (ivanpzv8@gmail.com)                                 

## [License](LICENSE)                                              

Copyright (c) 2019 Ivan Podmazov                                   

