#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget container (QWidget)
  ((widgets :initarg :widgets :accessor widgets))
  (:default-initargs :widgets ()))

(defgeneric widget (place container))
(defgeneric add-widget (widget container))
(defgeneric (setf widget) (widget place container))
(defgeneric insert-widget (widget place container))
(defgeneric remove-widget (place container))
(defgeneric update (container))

(defmethod widget ((n integer) (container container))
  (nth n (widgets container)))

(defmethod widget (widget (container container))
  (find widget (widgets container)))

(defmethod (setf widget) (widget (n integer) (container container))
  (setf (nth n container) widget))

(defmethod (setf widget) (widget find (container container))
  (loop for cell on (widgets container)
        when (eql (car cell) find)
        do (return (setf (car cell) widget))))

(defmethod (setf widget) :around (widget place (container container))
  (call-next-method)
  (update container))

(defmethod add-widget (widget (container container))
  (push widget (widgets container))
  (setf (parent widget) container))

(defmethod add-widget :around (widget (container container))
  (call-next-method)
  (update container))

(defmethod insert-widget (widget (n integer) (container container))
  (let ((cell (nthcdr n (widgets container))))
    (setf (cdr cell) (cons (car cell) (cdr cell))
          (car cell) widget))
  (setf (parent widget) container))

(defmethod insert-widget (widget (find widget) (container container))
  (insert-widget widget (position widget (widgets container)) container))

(defmethod insert-widget :around (widget place (container container))
  (call-next-method)
  (update container))

(defmethod remove-widget ((n integer) (container container))
  (cond ((= 0 n)
         (setf (parent (first (widgets container))) NIL)
         (setf (widgets container) (rest (widgets container))))
        (T
         (let ((cell (nthcdr (1- n) (widgets container))))
           (setf (parent (cadr cell)) NIL)
           (setf (cdr cell) (cddr cell))))))

(defmethod remove-widget (widget (container container))
  (remove-widget (position widget (widgets container)) container))

(defmethod remove-widget :around (place (container container))
  (call-next-method)
  (update container))

(defmethod update ((container container)))
