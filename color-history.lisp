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
  (let ((current (direct-value (widget 0 color-history))))
    (unless (or (eql current value)
                (and (= (q+:red-f current) (q+:red-f value))
                     (= (q+:green-f current) (q+:green-f value))
                     (= (q+:blue-f current) (q+:blue-f value))))
      (rotate-colors color-history)
      (setf (value (widget 0 color-history)) value))))

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
        (dotimes (i (- delta)) (rotate-right)))
    (update color-history)))

(define-widget color-history-swatch (QPushButton color-storing-input)
  ())

(define-initializer (color-history-swatch setup)
  (setf (q+:flat color-history-swatch) T)
  (setf (q+:auto-fill-background color-history-swatch) T))

(define-override (color-history-swatch paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter color-history-swatch)))
    (q+:fill-rect painter (q+:rect color-history-swatch)
                  (slot-value color-history-swatch 'value))))

(define-override (color-history-swatch mouse-release-event) (ev)
  (qtenumcase (q+:button ev)
    ((q+:qt.left-button)
     (rotate-colors (parent color-history-swatch) +1)
     (setf (value (parent color-history-swatch)) (value (parent color-history-swatch))))
    ((q+:qt.right-button)
     (rotate-colors (parent color-history-swatch) -1)
     (setf (value (parent color-history-swatch)) (value (parent color-history-swatch))))))
