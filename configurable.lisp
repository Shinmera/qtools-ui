#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defclass configurable-class (standard-class)
  ((options :initarg :options :accessor configurable-class-options)
   (option-order :initarg :option-order :accessor configurable-class-option-order))
  (:default-initargs
   :options ()
   :option-order ()))

(defmethod c2mop:validate-superclass ((class configurable-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass configurable-class))
  T)

(defmethod c2mop:validate-superclass ((class configurable-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class configurable-class) (superclass configurable-class))
  T)

(defclass configurable-slot ()
  ((option :initarg :option :initform NIL :accessor configurable-slot-option))
  (:documentation "Superclass for configurable slots with an option"))

(defclass configurable-direct-slot-definition (configurable-slot c2mop:standard-direct-slot-definition)
  ())

(defclass configurable-effective-slot-definition (configurable-slot c2mop:standard-effective-slot-definition)
  ((direct-slot :initform NIL :accessor configurable-effective-slot-direct-slot)))

(defmethod c2mop:direct-slot-definition-class ((class configurable-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'configurable-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class configurable-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'configurable-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class configurable-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (dolist (direct-slot direct-slots)
      (when (and (typep direct-slot 'configurable-direct-slot-definition)
                 (eql (c2mop:slot-definition-name direct-slot)
                      (c2mop:slot-definition-name effective-slot)))
        (setf (slot-value effective-slot 'option)
              (copy-list (configurable-slot-option direct-slot)))
        (setf (configurable-effective-slot-direct-slot effective-slot) direct-slot)
        (return)))
    effective-slot))

(defmethod c2mop:finalize-inheritance :after ((class configurable-class))
  ;; Make sure the slots get the options.
  (loop for (name . option) in (configurable-class-options class)
        for slot = (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name)
        do (unless slot
             (error "Defined option on slot ~s which does not exist on class ~a." name class))
           (unless (typep slot 'configurable-slot)
             (error "Defined option on slot ~s which is not a configurable-slot on class ~a." name class))
           (setf (configurable-slot-option slot) option)))

(defclass configurable ()
  ()
  (:metaclass configurable-class))

(defun coerce-option-for-slot (option slot)
  (let ((slot-name (c2mop:slot-definition-name slot)))
    (unless (getf option :small)
      (setf (getf option :small) T))
    ;; Almost the same twice.
    (unless (getf option :reader)
      (case (or (getf option :accessor-type) :accessor)
        (:accessor
         (let ((reader (first (c2mop:slot-definition-readers
                               (configurable-effective-slot-direct-slot slot)))))
           (cond (reader 
                  (setf (getf option :reader) reader))
                 (T
                  (setf (getf option :reader) (lambda (target)
                                                (slot-value target slot-name)))))))
        (:slot (setf (getf option :writer) slot-name))
        (:function (setf (getf option :reader) (lambda (target)
                                                 (slot-value target slot-name))))))
    (unless (not (getf option :writer))
      (case (or (getf option :accessor-type) :accessor)
        (:accessor
         (let ((writer (first (c2mop:slot-definition-writers
                               (configurable-effective-slot-direct-slot slot)))))
           (cond (writer
                  (setf (getf option :writer) writer))
                 (T
                  (setf (getf option :writer) (lambda (target value)
                                                (setf (slot-value target slot-name) value)))))))
        (:slot (setf (getf option :writer) slot-name))
        (:function (setf (getf option :writer) (lambda (target value)
                                                 (setf (slot-value target slot-name) value)))))))
  option)

(defgeneric configuration-container (configurable)
  (:method ((configurable configurable))
    (let ((option-container (make-instance 'option-container)))
      (loop for slot-name in (configurable-class-option-order (class-of configurable))
            for slot = (find slot-name (c2mop:class-slots (class-of configurable))
                             :key #'c2mop:slot-definition-name)
            for (type . args) = (configurable-slot-option slot)
            do (when type
                 (add-item (apply #'make-option type :target configurable (coerce-option-for-slot args slot))
                             option-container)))
      option-container)))

(defmacro define-configurable (name direct-superclasses direct-slots &rest options)
  (unless (find :metaclass options :key #'car)
    (push `(:metaclass configurable-class) options))
  (push 'configurable direct-superclasses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
