#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric resize-widget (place size splitter))
(defgeneric orientation (splitter))
(defgeneric (setf orientation) (orientation splitter))
(defgeneric handle-size (splitter))
(defgeneric (setf handle-size) (handle-size splitter))

(define-widget splitter (QWidget container)
  ((orientation :initarg :orientation :accessor orientation)
   (handles :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor handles)
   (handle-size :initarg :handle-size :accessor handle-size))
  (:default-initargs
    :orientation :vertical
    :handle-size 5))

(defmethod (setf orientation) :after (value (splitter splitter))
  (update splitter))

(defmethod (setf handle-size) :after (value (splitter splitter))
  (update splitter))

(defmethod add-widget :after (widget (splitter splitter))
  (vector-push-extend (make-instance 'splitter-handle :widget widget :splitter splitter)
                      (handles splitter)))

(defmethod insert-widget :after ((n integer) widget (splitter splitter))
  (insert (make-instance 'splitter-handle :widget widget :splitter splitter) n (handles splitter)))

(defmethod remove-widget :after ((n integer) (splitter splitter))
  (finalize (remove-nth n (handles splitter))))

(defmethod swap-widgets :after ((a integer) (b integer) (splitter splitter))
  (rotatef (elt (handles splitter) a) (elt (handles splitter) b)))

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
     (let ((y 0) (i 0))
       (do-widgets (widget splitter)
         (let ((handle (aref (handles splitter) i)))
           (setf (q+:geometry widget) (values 0 y (q+:width splitter) (q+:height widget)))
           (setf (q+:geometry handle) (values 0 (+ y (q+:height widget)) (q+:width splitter) (handle-size splitter)))
           (incf y (+ (q+:height widget) (handle-size splitter)))
           (incf i)))
       (setf (q+:minimum-height splitter) y)))
    (:horizontal
     (let ((x 0) (i 0))
       (do-widgets (widget splitter)
         (let ((handle (aref (handles splitter) i)))
           (setf (q+:geometry widget) (values x 0 (q+:width splitter) (q+:height widget)))
           (setf (q+:geometry handle) (values (+ x (q+:width widget)) 0 (q+:width splitter) (handle-size splitter)))
           (incf x (+ (q+:width widget) (handle-size splitter)))
           (incf i)))
       (setf (q+:minimum-width splitter) x)))))


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
