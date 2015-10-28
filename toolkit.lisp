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
