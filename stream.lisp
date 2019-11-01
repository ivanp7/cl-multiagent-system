;;;; stream.lisp

(in-package #:cl-multiagent-system)

(defparameter *default-buffer-size* 65536)
(defparameter *default-buffer-element-type* '(unsigned-byte 8))

(defun make-stream-sender (serializer stream stream-lock)
  (let (buffer)
    (lambda (id msg)
      (when (open-stream-p stream) 
        (multiple-value-bind (new-buffer serialized-p)
            (funcall serializer id msg buffer)
          (setf buffer new-buffer)
          (when serialized-p
            (bt:with-lock-held (stream-lock)
              (write-sequence buffer stream))
            t))))))

(defun make-stream-receiver 
    (deserializer stream stream-lock 
     &optional (buffer-size *default-buffer-size*) 
               (buffer-element-type *default-buffer-element-type*))
  (let ((buffer (make-array `(,buffer-size) :element-type buffer-element-type))
        (buffer-lock (bt:make-lock)) (buffer-start 0) (buffer-end 0) 
        buffer-changed)
    (values
      ;; receive bytes from stream
      (lambda ()
        (bt:with-lock-held (stream-lock)
          (when (listen stream)
            (bt:with-lock-held (buffer-lock)
              (let ((old-end buffer-end))
                (setf buffer-end (read-sequence buffer stream 
                                                :start buffer-end)
                      buffer-changed (or buffer-changed
                                         (/= buffer-end old-end))))))))
      ;; deserialize received bytes
      (lambda ()
        (bt:with-lock-held (buffer-lock)
          (when buffer-changed
            (setf buffer-changed nil)
            (multiple-value-bind (id msg deserialized-length)
                (funcall deserializer buffer buffer-start buffer-end)
              (when (and deserialized-length (plusp deserialized-length))
                (incf buffer-start deserialized-length)
                (cond 
                  ((= buffer-start buffer-end)
                   (setf buffer-start 0 buffer-end 0))
                  ((and (= buffer-end buffer-size) (plusp buffer-start))
                   (decf buffer-end buffer-start)
                   (dotimes (i buffer-end)
                     (setf (aref buffer i) (aref buffer (+ i buffer-start))))
                   (setf buffer-start 0))
                  ((and (= buffer-end buffer-size) (zerop buffer-start))
                   (setf buffer-size (* 2 buffer-size))
                   (adjust-array buffer `(,buffer-size))))
                (values t id msg)))))))))

