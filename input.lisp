#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defvar *recursive-input-set* NIL)

(define-widget input (QWidget repaintable)
  ())

(define-signal (input input-updated) ())
(define-signal (input input-done) ())

(defmethod (setf value) :around (value (input input))
  ;; Yes yes, O(n). However, I don't expect -- or rather, I hope -- that
  ;; the nesting of input widgets is never going to be very deep.
  (unless (find input *recursive-input-set*)
    (let ((*recursive-input-set* (list* input *recursive-input-set*)))
      (call-next-method)
      (repaint input)
      (signal! input (input-updated)))))
