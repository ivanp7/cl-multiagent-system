;;;; stream.lisp

(in-package #:cl-multiagent-system)

(defun make-stream-sender (serializer stream lock)
  (let (buffer)
    (lambda (id msg)
      (when (open-stream-p stream) 
        (multiple-value-bind (new-buffer serialized-p)
            (funcall serializer id msg buffer)
          (setf buffer new-buffer)
          (when serialized-p
            (bt:with-lock-held (lock)
              (write-sequence buffer stream))
            t))))))

(defun make-stream-receiver (deserializer stream lock &optional (buffer-size 1024) 
                      (buffer-element-type '(unsigned-byte 8)))
  (let ((buffer (make-array `(,buffer-size) :element-type buffer-element-type))
        (buffer-pos 0))
    (lambda ()
      (setf buffer-pos (bt:with-lock-held (lock)
                         (read-sequence buffer stream :start buffer-pos)))
      (when (= buffer-pos buffer-size)
        (setf buffer-size (* 2 buffer-size))
        (adjust-array buffer `(,buffer-size)))
      (multiple-value-bind (id msg deserialized-length)
          (funcall deserializer buffer)
        (when (and deserialized-length (plusp deserialized-length))
          (dotimes (i (- buffer-pos deserialized-length))
            (setf (aref buffer i) (aref buffer (+ i deserialized-length))))
          (setf buffer-pos 0)
          (values t id msg))))))

