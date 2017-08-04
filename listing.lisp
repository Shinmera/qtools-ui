#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric minimum-row-height (listing))
(defgeneric (setf minimum-row-height) (value listing))
(defgeneric fixed-row-height (listing))
(defgeneric (setf fixed-row-height) (value listing))
(defgeneric draggable (listing))
(defgeneric (setf draggable) (value listing))

(define-widget listing (QWidget sorted-item-container selectable-layout)
  ((minimum-row-height :initarg :minimum-row-height :accessor minimum-row-height)
   (fixed-row-height :initarg :fixed-row-height :accessor fixed-row-height)
   (draggable :initarg :draggable :accessor draggable))
  (:default-initargs
    :minimum-row-height 20
    :fixed-row-height NIL
    :draggable T))

(defun listing-widget-height (listing widget)
  (or (fixed-row-height listing)
      (max (minimum-row-height listing)
           (q+:minimum-height widget)
           (q+:height (q+:size-hint widget)))))

(defun listing-useful-width (listing)
  (let ((parent (parent listing)))
    (if parent (q+:width parent) (q+:width listing))))

(defmethod update ((listing listing))
  (let ((y 0) (w (listing-useful-width listing)))
    (do-widgets (widget listing)
      (when (or (not (q+:is-visible listing))
                (q+:is-visible widget))
        (let ((height (listing-widget-height listing widget)))
          (setf (q+:geometry widget) (values 0 y w height))
          (incf y height))))))

(defmethod update-for-added ((new widget) (listing listing))
  (let ((y 0) (w (listing-useful-width listing)) (found))
    (do-widgets (widget listing)
      (when (eql widget new)
        (setf found T))
      (when (or (not (q+:is-visible listing))
                (q+:is-visible widget))
        (let ((height (listing-widget-height listing widget)))
          (when found
            (setf (q+:geometry widget) (values 0 y w height)))
          (incf y height))))
    (q+:update-geometry listing)))

(defmethod update-for-removed ((old widget) (listing listing))
  (let ((y 0) (w (listing-useful-width listing)) (found))
    (do-widgets (widget listing)
      (if (eql widget old)
          (setf found T)
          (when (or (not (q+:is-visible listing))
                    (q+:is-visible widget))
            (let ((height (listing-widget-height listing widget)))
              (when found
                (setf (q+:geometry widget) (values 0 y w height)))
              (incf y height)))))
    (q+:update-geometry listing)))

(define-override (listing size-hint) ()
  (let ((w 100)
        (h 0))
    (do-widgets (widget listing)
      (when (q+:is-visible widget)
        (let ((width (max 30 (q+:minimum-width widget) (q+:width (q+:size-hint widget))))
              (height (listing-widget-height listing widget)))
          (setf w (max w width))
          (incf h height))))
    (q+:make-qsize w h)))

(define-override (listing minimum-size-hint) ()
  (let ((w 0)
        (h 0))
    (do-widgets (widget listing)
      (when (q+:is-visible widget)
        (let* ((hint (q+:minimum-size-hint widget))
               (width (max (q+:minimum-width widget) (q+:width hint)))
               (height (max (q+:minimum-height widget) (q+:height hint))))
          (setf w (max w width))
          (incf h height))))
    (q+:make-qsize w h)))

(defmethod (setf sorting) :after (sorting (listing listing))
  (when sorting (setf (draggable listing) NIL)))

(defmethod coerce-item (item (listing listing))
  (make-instance 'listing-item :item item :container listing))

(defmethod widget-acceptable-p ((widget qobject) (listing listing))
  NIL)

(define-widget listing-item (QWidget cell)
  ())

(defmethod drag :around ((listing-item listing-item) px py nx ny)
  (when (draggable (container listing-item))
    (call-next-method)))

(defmethod widget-acceptable-p ((listing-item listing-item) (listing listing))
  T)
