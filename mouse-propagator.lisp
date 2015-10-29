#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget mouse-propagator (QObject)
  ((target :initarg :target :accessor target)))

(define-initializer (mouse-propagator setup) ()
  (unless (slot-boundp mouse-propagator 'target)
    (setf (target mouse-propagator) mouse-propagator)))

(define-override (mouse-propagator event-filter) (obj ev)
  (qtenumcase (enum-value (q+:type ev))
    ((or (q+:qevent.mouse-button-press) (q+:qevent.mouse-button-dbl-click)
         (q+:qevent.mouse-button-release) (q+:qevent.mouse-move))
     (q+:qcoreapplication-post-event (target mouse-propagator)
                                     (copy (qt:cast "QMouseEvent" ev)))))
  ;; Do process it in the actual widget.
  NIL)
