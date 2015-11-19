#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defvar *recursive-input-set* NIL)

(define-widget input (QWidget repaintable)
  ())

(define-signal (input input-updated) ())
(define-signal (input input-done) ())

(defmethod (setf value) :around (value (input input))
  ;; Yes yes, O(n). However, I don't expect -- or rather, I hope -- that
  ;; the nesting of input widgets is never going to be very deep.
  ;;
  ;; This also causes issues when you just want to delegate a value setting
  ;; to a later method as happens down there. The hack is to just POP the
  ;; *recursive-input-set*.
  (unless (find input *recursive-input-set*)
    (let ((*recursive-input-set* (list* input *recursive-input-set*)))
      (call-next-method)
      (repaint input)
      (signal! input (input-updated)))))

(define-widget storing-input (QWidget input)
  ((value :initarg :value :accessor direct-value :accessor value)))

(defmethod (setf value) :around (thing (storing-input storing-input))
  (unless (equal thing (value storing-input))
    (call-next-method)))

(define-widget color-storing-input (QWidget storing-input)
  ((color-type :initform :rgb :reader color-type :allocation :class)
   (value :initform (q+:make-qcolor) :accessor direct-value :accessor value :finalized T))
  (:default-initargs
    :color (c 0 0 0)))

(defmethod initialize-instance :after ((color-storing-input color-storing-input) &key color &allow-other-keys)
  (setf (value color-storing-input) color))

(defmethod reinitialize-instance :after ((color-storing-input color-storing-input) &key color &allow-other-keys)
  (setf (value color-storing-input) color))

(defmethod (setf value) ((rgba integer) (color-storing-input color-storing-input))
  (unless (= rgba (q+:rgba (direct-value color-storing-input)))
    (call-next-method)))

(defmethod (setf value) (thing (color-storing-input color-storing-input))
  (error "Don't know how to coerce ~s into a colour." thing))

(defmethod (setf value) ((rgba integer) (color-storing-input color-storing-input))
  (setf (q+:rgba (direct-value color-storing-input)) rgba))

(defmethod (setf value) ((color qobject) (color-storing-input color-storing-input))
  (unless (qtypep color "QColor")
    (error "~a is not a QColor." color))
  (ecase (color-type color-storing-input)
    (:rgb
     (setf (q+:rgb-f (direct-value color-storing-input))
           (values (q+:red-f color) (q+:green-f color) (q+:blue-f color) (q+:alpha-f color))))
    (:hsv
     (setf (q+:hsv-f (direct-value color-storing-input))
           (values (if (= (q+:hsv-hue-f color) -1)
                       (q+:hsv-hue-f (direct-value color-storing-input))
                       (q+:hsv-hue-f color))
                   (q+:saturation-f color)
                   (q+:value-f color))))))

(defmethod (setf value) ((color list) (color-storing-input color-storing-input))
  (destructuring-bind (r g b &optional (a 255)) color
    (pop *recursive-input-set*)
    (setf (value color-storing-input) (color-to-rgba r g b a))))

(defmethod value ((color-storing-input color-storing-input))
  (copy (direct-value color-storing-input)))
