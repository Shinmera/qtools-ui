#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@teknik.io>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defvar *mime-data-with-object-type* "application/qtools-mime-data-with-object")

(defgeneric drop (item target))
(defgeneric drop-acceptable-p (item target))

(define-widget mime-data-with-object (QMimeData)
  ((object :accessor object :initarg :object))
  (:default-initargs :object (error "OBJECT required.")))

(defmethod initialize-instance :after ((object mime-data-with-object) &key mime-type)
  (setf (q+:data object mime-type) ""))

(define-widget droppable (QWidget draggable)
  ((mime-type :accessor mime-type :initarg :mime-type))
  (:default-initargs :mime-type *mime-data-with-object-type*))

(defmethod drag-start :before ((droppable droppable) x y)
  (let ((drag (q+:make-qdrag droppable))
        (mime-data (make-instance 'mime-data-with-object
                                  :object droppable
                                  :mime-type (mime-type droppable))))
    (setf (q+:mime-data drag) mime-data)
    (let ((drop-action (q+:exec drag (q+:qt.move-action))))
      (when (= (enum-value drop-action) (q+:qt.ignore-action))
        (q+:delete-later drag)
        (q+:delete-later mime-data)))))

(define-widget drop-target (QWidget)
  ((mime-type :accessor mime-type :initarg :mime-type))
  (:default-initargs :mime-type *mime-data-with-object-type*))

(define-initializer (drop-target initialize-drop-target)
  (setf (q+:accept-drops drop-target) t))

(define-override (drop-target drag-enter-event) (event)
  (when (q+:has-format (q+:mime-data event) mime-type)
    (q+:accept-proposed-action event))
  (stop-overriding))

(define-override (drop-target drop-event) (event)
  (let ((mime-data (q+:mime-data event)))
    (q+:accept-proposed-action event)
    (drop mime-data drop-target))
  (stop-overriding))

(defmethod drop-acceptable-p (item target)
  NIL)

(defmethod drop-acceptable-p ((item mime-data-with-object) target)
  T)

(defmethod drop :around (item target)
  (when (drop-acceptable-p item target)
    (call-next-method)))

(defmethod drop ((item mime-data-with-object) target)
  (drop (object item) target))
