#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget splitter-handle (QWidget draggable)
  ((widget :initarg :widget)
   (splitter :initarg :splitter))
  (:default-initargs
    :widget (error "WIDGET required.")
    :splitter (error "SPLITTER required.")))

(define-initializer (splitter-handle setup)
  (setf (q+:cursor splitter-handle) (q+:make-qcursor
                                     (ecase (orientation splitter)
                                       (:vertical (q+:qt.split-vcursor))
                                       (:horizontal (q+:qt.split-hcursor)))))
  (setf (parent splitter-handle) splitter)
  (setf (q+:auto-fill-background splitter-handle) T)
  (setf (q+:color (q+:palette splitter-handle) (q+:qpalette.background))
        (q+:darker (q+:color (q+:palette splitter-handle) (q+:qpalette.background)) 150)))

(defmethod drag ((splitter-handle splitter-handle) px py nx ny)
  (with-slots-bound (splitter-handle splitter-handle)
    (resize-widget widget (cons (+ (q+:width widget) (- nx px))
                                (+ (q+:height widget) (- ny py)))
                   splitter)))

(define-widget splitter (QWidget container)
  ((orientation :initarg :orientation :accessor orientation)
   (handles :initform NIL :accessor handles)
   (handle-size :initarg :handle-size :accessor handle-size))
  (:default-initargs
    :orientation :vertical
    :handle-size 5))

(define-override (splitter resize-event) (ev)
  (update splitter)
  (stop-overriding))

(define-override (splitter event) (ev)
  (when (= (enum-value (q+:type ev)) (q+:qevent.layout-request))
    (update splitter))
  (stop-overriding))

(defmethod add-widget :after (widget (splitter splitter))
  (push (make-instance 'splitter-handle :widget widget :splitter splitter)
        (handles splitter)))

(defmethod insert-widget :after ((n integer) widget (splitter splitter))
  (insert (make-instance 'splitter-handle :widget widget :splitter splitter) n (handles splitter)))

(defmethod remove-widget :after ((n integer) (splitter splitter))
  (finalize (remove-nth n (handles splitter))))

(defmethod swap-widget :after ((a integer) (b integer) (splitter splitter))
  (swapcar a b (handles splitter)))

(defgeneric resize-widget (place size splitter))

(defmethod resize-widget ((n integer) size (splitter splitter))
  (resize-widget (widget n splitter) size splitter))

(defmethod resize-widget (widget size (splitter splitter))
  (let ((w (clamp (q+:minimum-width widget) (if (consp size) (car size) size) (q+:maximum-width widget)))
        (h (clamp (q+:minimum-height widget) (if (consp size) (cdr size) size) (q+:maximum-height widget))))
    (setf (q+:geometry widget) (values (q+:x widget) (q+:y widget) w h))
    (update splitter)))

(defmethod resize-widget :after (widget size (splitter splitter))
  (update splitter))

(defmethod update ((splitter splitter))
  (ecase (orientation splitter)
    (:vertical
     (loop for y = 0 then (+ y (q+:height widget) (handle-size splitter))
           for widget in (widgets splitter)
           for handle in (handles splitter)
           do (setf (q+:geometry widget) (values 0 y (q+:width splitter) (q+:height widget)))
              (setf (q+:geometry handle) (values 0 (+ y (q+:height widget)) (q+:width splitter) (handle-size splitter)))
           finally (setf (q+:minimum-height splitter) y)))
    (:horizontal
     (loop for x = 0 then (+ x (q+:width widget) (handle-size splitter))
           for widget in (widgets splitter)
           for handle in (handles splitter)
           do (setf (q+:geometry widget) (values x 0 (q+:width widget) (q+:height splitter)))
              (setf (q+:geometry handle) (values (+ x (q+:width widget)) 0 (handle-size splitter) (q+:height splitter)))
           finally (setf (q+:minimum-width splitter) x)))))
