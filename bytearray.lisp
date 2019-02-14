#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

;;; We (ab)use fast-calls to pass around pointers that the usual
;;; CommonQt would mistakenly try to translate to strings.

(defmacro define-hot-patching (name args &body body)
  (let ((first-args (gensym "ARGUMENTS")))
    `(defun ,name (&rest ,first-args)
       (apply
        (compile ',name '(lambda ,args
                          ,@body))
        ,first-args))))

(define-hot-patching from-byte-array (bytes &optional (finalize T))
  (prog1 (cffi:foreign-array-to-lisp
          (fast-call (data qbytearray "void**") bytes)
          `(:array :uchar ,(fast-call (length qbytearray "int") bytes))
          :element-type '(unsigned-byte 8))
    (when finalize (optimized-delete bytes))))

(define-hot-patching to-byte-array (data)
  (cffi:with-pointer-to-vector-data (pointer data)
    (let ((bytes (fast-static-call (qbytearray-from-raw-data "QByteArray" "const char*" "int")
                                   pointer (length data))))
      (fast-call (detach qbytearray) bytes)
      bytes)))
