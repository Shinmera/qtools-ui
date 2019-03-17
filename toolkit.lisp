#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defun stable-sort-into (sequence predicate &rest args &key key)
  (declare (ignore key))
  (let ((sorted (apply #'stable-sort sequence predicate args)))
    (unless (eq sorted sequence)
      (map-into sequence #'identity (if (listp sorted) (copy-list sorted) sorted)))
    sequence))

(defun swapcar (a b list)
  (when (< b a) (rotatef a b))
  (let* ((acell (nthcdr a list))
         (bcell (nthcdr (- b a) acell))
         (first (car acell)))
    (setf (car acell) (car bcell)
          (car bcell) first))
  list)

(defun insert (item pos sequence)
  (etypecase sequence
    (list
     (let ((cell (nthcdr pos sequence)))
       (setf (cdr cell) (cons (car cell) (cdr cell))
             (car cell) item)))
    (vector
     (array-utils:vector-push-extend-position item sequence pos)))
  sequence)

(defmacro remove-nth (pos sequence)
  (let ((n (gensym "POS"))
        (cell (gensym "CELL"))
        (seq (gensym "SEQ")))
    `(let ((,n ,pos)
           (,seq ,sequence))
       (etypecase ,seq
         (list
          (if (= 0 ,n)
                   (pop ,seq)
                   (let ((,cell (nthcdr (1- ,n) ,seq)))
                     (prog1 (cadr ,cell)
                       (setf (cdr ,cell) (cddr ,cell))))))
         (vector
          (array-utils:vector-pop-position ,seq ,n))))))

(defun rotate-seq (sequence &optional (delta 1))
  (etypecase sequence
    (list
     (when sequence
       (flet ((rotate-left ()
                (let ((last (last sequence)))
                  (setf (cdr last) (butlast sequence)
                        sequence last)))
              (rotate-right ()
                (let ((first sequence))
                  (setf (cdr (last sequence)) first
                        sequence (cdr first)
                        (cdr first) NIL))))
         (if (< 0 delta)
             (dotimes (i delta) (rotate-left))
             (dotimes (i (- delta)) (rotate-right))))))
    (vector
     (when (< 0 (length sequence))
       (flet ((rotate-left ()
                (let ((last (aref sequence (1- (length sequence)))))
                  (loop for i downfrom (1- (length sequence)) above 0
                        do (setf (aref sequence i) (aref sequence (1- i)))
                        finally (setf (aref sequence 0) last))))
              (rotate-right ()
                (let ((first (aref sequence 0)))
                  (loop for i from 0 below (1- (length sequence))
                        do (setf (aref sequence i) (aref sequence (1+ i)))
                        finally (setf (aref sequence (1- (length sequence))) first)))))
         (let ((delta (mod delta (length sequence))))
           (if (< 0 delta)
               (dotimes (i delta) (rotate-left))
               (dotimes (i (- delta)) (rotate-right))))))))
  sequence)

(defmacro rotate-seqf (sequence &optional (delta 1))
  `(setf ,sequence (rotate-seq ,sequence ,delta)))

(defun clamp (low mid high)
  (min (max mid low) high))

(defun default-test (test test-not)
  (if (and (not test) (not test-not))
      #'eql
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
    (setf (ldb (byte 8 0) rgba) (round b)
          (ldb (byte 8 8) rgba) (round g)
          (ldb (byte 8 16) rgba) (round r)
          (ldb (byte 8 24) rgba) (round a))
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
        (setf (gethash rgba *color-map*)
              (q+:make-qcolor (round r) (round g) (round b) (round a))))))

(defun coerce-color (color)
  (etypecase color
    (qobject
     color)
    (integer
     (multiple-value-bind (r g b a) (rgba-to-color color)
               (c r g b a)))
    (cons
     (destructuring-bind (r g b &optional (a 255)) color
            (c r g b a)))))

(defun ensure-function (function-ish)
  (etypecase function-ish
    (cl:function function-ish)
    (symbol (fdefinition function-ish))
    (list (case (first function-ish)
            ((cl:lambda) (compile NIL function-ish))
            ((cl:setf cl+qt:setf) (fdefinition function-ish))
            ((cl:function cl+qt:function) (eval function-ish))
            (T (error "Don't know how to turn ~s into a function." function-ish))))))

(defun read-symbol (string &optional (case :upcase))
  (let ((out (make-string-output-stream))
        (package NIL)
        (name NIL)
        (escape NIL))
    ;; I know this is not entirely right as to how symbols are read, but we are
    ;; being lenient on purpose.
    (loop for c across string
          do (cond (escape (write-char c out)
                           (setf escape NIL))
                   (T (case c
                        (#\\ (setf escape T))
                        (#\: (cond (package (write-char c out))
                                   (T (setf package (get-output-stream-string out)))))
                        (T (case case
                             (:upcase (write-char (char-upcase c) out))
                             (:downcase (write-char (char-downcase c) out))
                             (:preserve (write-char c out))
                             (:invert (cond ((upper-case-p c) (write-char (char-downcase c) out))
                                            ((lower-case-p c) (write-char (char-upcase c) out))
                                            (T (write-char c out)))))))))
          finally (setf name (get-output-stream-string out)))
    (cond ((string= package "#")
           (make-symbol name))
          ((string= package "")
           (intern name :keyword))
          (T
           (intern name package)))))

(defun shortest-package-name (package)
  (let ((package (find-package package))
        (minimal (package-name package)))
    (loop for name in (package-nicknames package)
          do (when (< (length name) (length minimal))
               (setf minimal name)))
    minimal))

(defun format-symbol (symbol &optional (case :upcase))
  (with-output-to-string (out)
    (flet ((format-string (string)
             (loop for c across string
                   do (cond ((char= c #\:)
                             (write-string "\\:" out))
                            ((upper-case-p c)
                             (case case
                               (:upcase (write-char (char-downcase c) out))
                               (:downcase (write-char #\\ out) (write-char c out))
                               (:preserve (write-char c out))
                               (:invert (write-char (char-downcase c) out))))
                            ((lower-case-p c)
                             (case case
                               (:upcase (write-char #\\ out) (write-char c out))
                               (:downcase (write-char c out))
                               (:preserve (write-char c out))
                               (:invert (write-char (char-upcase c) out))))
                            (T (write-char c out))))))
      (let ((package (symbol-package symbol)))
        (cond ((eql (find-package :keyword) package))
              (package
               (format-string (shortest-package-name package)))
              (T
               (write-char #\# out))))
      (write-char #\: out)
      (format-string (symbol-name symbol)))))

(defun hue-shift (image rotation)
  (declare (optimize speed))
  (declare (type (single-float 0.0 (360.0)) rotation)
           (type qobject image))
  (assert (qtypep image "QImage") (image)
          "The value of IMAGE is not a QImage: ~A" image)
  (assert (= (the fixnum (qt:enum-value (q+:format image)))
             (the fixnum (q+:qimage.format_argb32))))
  (let ((pointer (q+:bits image))
        (total-size (* (the (unsigned-byte 16) (q+:width image))
                       (the (unsigned-byte 16) (q+:height image)))))
    (dotimes (i total-size)
      (let* ((uint (cffi:mem-aref pointer :unsigned-int i))
             (rgb (make-array 3 :element-type '(unsigned-byte 8)
                                :initial-contents
                                (list (ldb (byte 8 0) uint)
                                      (ldb (byte 8 8) uint)
                                      (ldb (byte 8 16) uint))))
             (rgb2 (the (simple-array (unsigned-byte 8) (3))
                        (rgb:rotate-rgb rgb rotation))))
        (setf (cffi:mem-aref pointer :unsigned-int i)
              (+ (aref rgb2 0)
                 (ash (aref rgb2 1) 8)
                 (ash (aref rgb2 2) 16)
                 (ash 255 24)))))))
