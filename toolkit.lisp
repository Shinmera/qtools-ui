#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defun swapcar (a b list)
  (when (< b a) (rotatef a b))
  (let* ((acell (nthcdr a list))
         (bcell (nthcdr (- b a) acell))
         (first (car acell)))
    (setf (car acell) (car bcell)
          (car bcell) first))
  list)

(defun insert (item pos list)
  (let ((cell (nthcdr pos list)))
    (setf (cdr cell) (cons (car cell) (cdr cell))
          (car cell) item))
  list)

(defmacro remove-nth (pos list)
  (let ((n (gensym "POS"))
        (cell (gensym "CELL")))
    `(let ((,n ,pos))
       (if (= 0 ,n)
           (pop ,list)
           (let ((,cell (nthcdr (1- ,n) ,list)))
             (prog1 (cadr ,cell)
               (setf (cdr ,cell) (cddr ,cell))))))))

(defun clamp (low mid high)
  (min (max mid low) high))

(defun default-test (test test-not)
  (if (and (not test) (not test-not))
      #'identity
      test))

(defun call-with-translation (painter target function)
  (q+:save painter)
  (q+:translate painter target)
  (unwind-protect
       (funcall function)
    (q+:restore painter)))

(defmacro with-translation ((painter target) &body body)
  `(call-with-translation ,painter ,target (lambda () ,@body)))

(defun color-to-rgba (r g b &optional (a 255))
  (let ((rgba 0))
    (setf (ldb (byte 8 0) rgba) b
          (ldb (byte 8 8) rgba) g
          (ldb (byte 8 16) rgba) r
          (ldb (byte 8 24) rgba) a)
    rgba))

(defun rgba-to-color (rgba)
  (values (ldb (byte 8 16) rgba)
          (ldb (byte 8 8) rgba)
          (ldb (byte 8 0) rgba)
          (ldb (byte 8 24) rgba)))

(defvar *color-map* (make-hash-table :test 'eql))

(defun c (r g b &optional (a 255))
  (let ((rgba (color-to-rgba r g b a)))
    (or (gethash rgba *color-map*)
        (setf (gethash rgba *color-map*) (q+:make-qcolor r g b a)))))

(defun coerce-color (color)
  (etypecase color
    (qobject color)
    (cons (c (first color) (second color) (third color)))))
