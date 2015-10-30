#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget draggable (QWidget)
  ((dragging :initform NIL :accessor dragging)))

(defgeneric drag-start (widget x y))
(defgeneric drag (widget from-x from-y to-x to-y))
(defgeneric drag-end (widget x y))

(define-override (draggable mouse-press-event) (ev)
  (when (and (not dragging)
             (= (enum-value (q+:button ev)) (q+:qt.left-button)))
    (setf dragging (list (q+:x ev) (q+:y ev)))
    (drag-start draggable (q+:x ev) (q+:y ev)))
  (stop-overriding))

(define-override (draggable mouse-release-event) (ev)
  (when (= (enum-value (q+:button ev)) (q+:qt.left-button))
    (setf dragging NIL)
    (drag-end draggable (q+:x ev) (q+:y ev)))
  (stop-overriding))

(define-override (draggable mouse-move-event) (ev)
  (when dragging
    (destructuring-bind (x y) dragging
      (drag draggable x y (q+:x ev) (q+:y ev))))
  (stop-overriding))

(defmethod drag ((draggable draggable) px py nx ny)
  (declare (ignore draggable px py nx ny)))

(defmethod drag-start ((draggable draggable) x y)
  (declare (ignore draggable x y)))

(defmethod drag-end ((draggable draggable) x y)
  (declare (ignore draggable x y)))
