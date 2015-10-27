#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget compass (QWidget)
  ((north :initarg :north :accessor north-widget)
   (east :initarg :east :accessor east-widget)
   (south :initarg :south :accessor south-widget)
   (west :initarg :west :accessor west-widget)
   (center :initarg :center :accessor center-widget))
  (:default-initargs
    :north NIL
    :east NIL
    :south NIL
    :west NIL
    :center NIL))

(defmacro define-compass-set-wrapper (name)
  `(defmethod (setf ,name) :around (widget (compass compass))
     (when (,name compass)
       (setf (parent (,name compass)) NIL))
     (call-next-method)
     (when widget
       (setf (parent widget) compass))
     (update compass)))

(macrolet ((define-all (&rest names)
             `(progn ,@(loop for name in names
                             collect `(define-compass-set-wrapper ,name)))))
  (define-all north-widget east-widget south-widget west-widget center-widget))

(define-initializer (compass setup) ()
  (when north (setf (north-widget compass) north))
  (when east (setf (east-widget compass) east))
  (when south (setf (south-widget compass) south))
  (when west (setf (west-widget compass) west))
  (when center (setf (center-widget compass) center)))

(define-override (compass resize-event) (ev)
  (update compass)
  (stop-overriding))

(define-override (compass event) (ev)
  (when (= (enum-value (q+:type ev)) (q+:qevent.layout-request))
    (update compass))
  (stop-overriding))

(defmethod update ((compass compass))
  (with-slots-bound (compass compass)
    (let ((ns (when north (q+:size-hint north)))
          (es (when east (q+:size-hint east)))
          (ss (when south (q+:size-hint south)))
          (ws (when west (q+:size-hint west))))
      (when north
        (setf (q+:geometry north) (values 0 0
                                          (q+:width compass) (q+:height ns))))
      (when south
        (setf (q+:geometry south) (values 0 (- (q+:height compass) (q+:height north))
                                          (q+:width compass) (q+:height ss))))
      (when west
        (setf (q+:geometry west) (values 0 (if north (q+:height north) 0)
                                         (q+:width ws) (- (q+:height compass) (if north (q+:height north) 0) (if south (q+:height south) 0)))))
      (when east
        (setf (q+:geometry east) (values (- (q+:width compass) (q+:width east)) (if north (q+:height north) 0)
                                         (q+:width es) (- (q+:height compass) (if north (q+:height north) 0) (if south (q+:height south) 0)))))
      (when center
        (setf (q+:geometry center) (values (if west (q+:width west) 0)
                                           (if north (q+:height north) 0)
                                           (- (q+:width compass) (if west (q+:width west) 0) (if east (q+:width east) 0))
                                           (- (q+:height compass) (if north (q+:height north) 0) (if south (q+:height south) 0))))))))
