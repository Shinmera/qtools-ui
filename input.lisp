#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defvar *recursive-input-set* NIL)

(define-widget input (QWidget)
  ())

(define-signal (input input-updated) ())

(defmethod (setf value) :around (value (input input))
  (unless (find input *recursive-input-set*)
    (let ((*recursive-input-set* (list* input *recursive-input-set*)))
      (call-next-method)
      (signal! input (input-updated)))))
