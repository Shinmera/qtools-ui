#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget color-history (QWidget flow-layout input)
  ((color-count :initarg :color-count :reader color-count))
  (:default-initargs
    :color-count 5))

(defmethod initialize-instance :after ((color-history color-history) &key)
  (when (< (color-count color-history) 1)
    (error "Invalid color-count ~s. Must be a positive integer." (color-count color-history)))
  (dotimes (i (color-count color-history))
    (add-widget (make-instance 'color-history-swatch) color-history)))

(defmethod value ((color-history color-history))
  (value (widget 0 color-history)))

(defmethod (setf value) (value (color-history color-history))
  (rotate-colors color-history)
  (setf (value (widget 0 color-history)) value))

(defun rotate-colors (color-history &optional (delta 1))
  (flet ((rotate-left ()
           (let ((last (last (widgets color-history))))
             (setf (cdr last) (butlast (widgets color-history))
                   (widgets color-history) last)))
         (rotate-right ()
           (let ((first (widgets color-history)))
             (setf (cdr (last (widgets color-history))) first
                   (widgets color-history) (cdr first)
                   (cdr first) NIL))))
    (if (< 0 delta)
        (dotimes (i delta) (rotate-left))
        (dotimes (i (- delta)) (rotate-right)))))

(define-widget color-history-swatch (QPushButton)
  ((color :initarg :color :accessor value))
  (:default-initargs
    :color (c 0 0 0)))

(define-initializer (color-history-swatch setup)
  (setf (q+:flat color-history-swatch) T)
  (setf (q+:auto-fill-background color-history-swatch) T)
  (setf (value color-history-swatch) (value color-history-swatch)))

(defmethod (setf value) :after (value (color-history-swatch color-history-swatch))
  (setf (q+:color (q+:palette color-history-swatch) (q+:qpalette.button))
        value))

(define-override (color-history-swatch mouse-release-event) (ev)
  (qtenumcase (q+:button ev)
    ((q+:qt.left-button)
     (setf (value (parent color-history-swatch)) (value color-history-swatch)))
    ((q+:qt.right-button)
     (rotate-colors (parent color-history-swatch) -1))))
