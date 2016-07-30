#|
 This file is a part of Qtools-UI
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-object executable (QObject)
  ((fill-queue :initform (make-array 0 :adjustable T :fill-pointer T) :reader fill-queue)
   (proc-queue :initform (make-array 0 :adjustable T :fill-pointer T) :reader proc-queue)
   (lock :initform (bt:make-lock) :reader lock)))

(define-signal (executable process-executions) ())

(define-slot (executable process-executions) ()
  (declare (connected executable (process-executions)))
  (bt:with-lock-held (lock)
    (rotatef fill-queue proc-queue))
  (loop for i from 0 below (length proc-queue)
        for execution = (aref proc-queue i)
        do (setf (aref proc-queue i) NIL)
           (when execution (execute execution)))
  (setf (fill-pointer proc-queue) 0))

(defmethod execute ((function cl:function))
  (funcall function))

(defmethod execute :around (thing)
  (with-simple-restart (abort "Abort executing ~a" thing)
    (call-next-method)))

(defmethod execute-in-gui (execution (executable executable))
  (bt:with-lock-held ((lock executable))
    (vector-push-extend execution (fill-queue executable))
    (signal! executable (process-executions))))

(defmacro with-body-in-gui ((executable) &body body)
  `(execute-in-gui (lambda () ,@body) ,executable))
