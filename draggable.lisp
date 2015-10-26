#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget draggable (QWidget)
  ((dragging :initform NIL :accessor dragging)))

(define-signal (draggable dragging) (int int int int))
(defgeneric drag (widget from-x from-y to-x to-y))

(define-override (draggable mouse-press-event) (ev)
  (when (and (not dragging)
             (= (enum-value (q+:button ev)) (q+:qt.left-button)))
    (setf dragging (list (q+:x ev) (q+:y ev))))
  (stop-overriding))

(define-override (draggable mouse-release-event) (ev)
  (when (= (enum-value (q+:button ev)) (q+:qt.left-button))
    (setf dragging NIL))
  (stop-overriding))

(define-override (draggable mouse-move-event) (ev)
  (when dragging
    (destructuring-bind (x y) dragging
      (signal! draggable (dragging int int int int) x y (q+:x ev) (q+:y ev))))
  (stop-overriding))

(define-slot (draggable drag drag) ((px int) (py int) (nx int) (ny int))
  (declare (connected draggable (dragging int int int int))))
