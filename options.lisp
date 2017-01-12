#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric option-effective-target (option))
(defgeneric option-target-value (option))
(defgeneric (setf option-target-value) (value option))
(defgeneric target (option))
(defgeneric (setf target) (target option))
(defgeneric reader (option))
(defgeneric (setf reader) (reader option))
(defgeneric writer (option))
(defgeneric (setf writer) (writer option))
(defgeneric title (option))
(defgeneric (setf title) (title option))
(defgeneric accessor-type (option))
(defgeneric (setf accessor-type) (accessor-type option))
(defgeneric option-updating (option))
(defgeneric (setf option-updating) (option-updating option))
(defgeneric option-small-p (option))
(defgeneric (setf option-small-p) (option-small-p option))
(defgeneric make-option (type &key &allow-other-keys))

(define-widget option (QWidget input)
  ((target :initarg :target :accessor target)
   (reader :initarg :reader :accessor reader)
   (writer :initarg :writer :accessor writer)
   (title :initarg :title :accessor title)
   (accessor-type :initarg :accessor-type :accessor accessor-type)
   (updating :initarg :updating :accessor option-updating)
   (small :initarg :small :accessor option-small-p))
  (:default-initargs
    :target (error "TARGET required.")
    :reader (error "READER required.")
    :writer NIL
    :accessor-type :accessor
    :updating :when-done
    :small NIL))

(defmethod option-effective-target ((option option))
  (let ((target (target option)))
    (typecase target
      (cl:function (funcall target))
      (symbol (symbol-value target))
      (T target))))

(defmethod generic-place-value (target accessor-type reader)
  (ecase accessor-type
    (:accessor (funcall (ensure-function reader) target))
    (:slot (slot-value target reader))
    (:function (funcall (ensure-function reader) target))
    (:hash-table (gethash reader target))
    (:sequence (elt target reader))
    (:value (value target))
    (:variable target)))

(defmethod (setf generic-place-value) (value target accessor-type writer &optional (reader writer))
  (ecase accessor-type
    (:accessor (funcall (ensure-function (or writer `(setf ,reader))) value target))
    (:slot (setf (slot-value target (or writer reader)) value))
    (:function (funcall (ensure-function (or writer reader)) value target))
    (:hash-table (setf (gethash (or writer reader) target) value))
    (:sequence (setf (elt target (or writer reader)) value))
    (:value (setf (value target) value))
    (:variable (setf (symbol-value target) value))))

(defmethod option-target-value ((option option))
  (generic-place-value (option-effective-target option)
                       (accessor-type option) (reader option)))

(defmethod (setf option-target-value) (value (option option))
  (setf (generic-place-value (option-effective-target option)
                             (accessor-type option) (writer option) (reader option))
        value))

(define-initializer (option setup)
  (setf (title option) (if (slot-boundp option 'title)
                           (title option)
                           (typecase (reader option)
                             (symbol (string (reader option)))
                             (T ""))))
  (setf (q+:minimum-height option) 20)
  (setf (value option) (option-target-value option)))

(defmethod (setf title) :after ((string string) (option option))
  (setf (q+:window-title option) string))

(define-slot (option input-done) ()
  (declare (connected option (input-done)))
  (when (eql updating :when-done)
    (setf (option-target-value option) (value option))))

(define-slot (option input-updated) ()
  (declare (connected option (input-updated)))
  (when (eql updating :on-change)
    (setf (option-target-value option) (value option))))


(define-widget boolean-option (QCheckBox option)
  ()
  (:default-initargs
    :small T))

(define-initializer (boolean-option setup)
  (connect! boolean-option (state-changed int) boolean-option (input-done))
  (connect! boolean-option (state-changed int) boolean-option (input-updated))
  (call-next-method))

(defmethod value ((boolean-option boolean-option))
  (q+:is-checked boolean-option))

(defmethod (setf value) (value (boolean-option boolean-option))
  (setf (q+:checked boolean-option) value))

(define-widget string-option (QLineEdit option)
  ()
  (:default-initargs
    :small T))

(define-initializer (string-option setup)
  (connect! string-option (editing-finished) string-option (input-done))
  (connect! string-option (text-changed string) string-option (input-updated))
  (call-next-method))

(defmethod value ((string-option string-option))
  (q+:text string-option))

(defmethod (setf value) (value (string-option string-option))
  (setf (q+:text string-option) (princ-to-string value)))

(define-widget password-option (QLineEdit string-option)
  ())

(define-initializer (password-option setup)
  (setf (q+:echo-mode password-option) (q+:qlineedit.password-echo-on-edit))
  (call-next-method))

(define-widget text-option (QPlainTextEdit option)
  ()
  (:default-initargs
    :small NIL))

(define-initializer (text-option setup)
  (connect! text-option (text-changed) text-option (input-updated))
  (call-next-method))

(define-override (text-option focus-out-event) (ev)
  (signal! text-option (input-done))
  (stop-overriding))

(defmethod value ((text-option text-option))
  (q+:to-plain-text text-option))

(defmethod (setf value) (value (text-option text-option))
  (setf (q+:plain-text text-option) (princ-to-string value)))

(define-widget integer-option (QSpinBox option)
  ()
  (:default-initargs
    :small T))

(define-initializer (integer-option setup)
  (setf (q+:range integer-option) (values (- (ash 1 31)) (1- (ash 1 31))))
  (connect! integer-option (editing-finished) integer-option (input-done))
  (connect! integer-option (value-changed integer) integer-option (input-updated))
  (call-next-method))

(defmethod initialize-instance :after ((integer-option integer-option) &key (min (- (ash 1 31)))
                                                                            (max (1- (ash 1 31))))
  (setf (q+:range integer-option) (values (max min (- (ash 1 31)))
                                          (min max (1- (ash 1 31))))))

;; FIXME: range
(define-widget double-option (QWidget slider option)
  ()
  (:default-initargs
    :small NIL))

(define-widget small-double-option (QDoubleSpinBox option)
  ()
  (:default-initargs
    :small T))

(define-initializer (small-double-option setup)
  (setf (q+:maximum small-double-option) most-positive-double-float
        (q+:minimum small-double-option) most-negative-double-float)
  (connect! small-double-option (editing-finished) small-double-option (input-done))
  (connect! small-double-option (value-changed double) small-double-option (input-updated))
  (call-next-method))

(defmethod initialize-instance :after ((small-double-option small-double-option) &key (min most-negative-double-float)
                                                                                      (max most-positive-double-float))
  (setf (q+:maximum small-double-option) (min max most-positive-double-float)
        (q+:minimum small-double-option) (max min most-negative-double-float)))

(define-widget complex-option (QWidget option)
  ()
  (:default-initargs
    :small T))

(define-subwidget (complex-option real) (q+:make-qdoublespinbox complex-option)
  (setf (q+:maximum real) most-positive-double-float)
  (setf (q+:minimum real) most-negative-double-float))

(define-subwidget (complex-option imag) (q+:make-qdoublespinbox complex-option)
  (setf (q+:maximum imag) most-positive-double-float)
  (setf (q+:minimum imag) most-negative-double-float))

(define-subwidget (complex-option layout) (q+:make-qhboxlayout complex-option)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout real)
  (q+:add-widget layout imag))

(defmethod value ((complex-option complex-option))
  (complex (q+:value (slot-value complex-option 'real))
           (q+:value (slot-value complex-option 'imag))))

(defmethod (setf value) (value (complex-option complex-option))
  (setf (q+:value (slot-value complex-option 'real)) (realpart value)
        (q+:value (slot-value complex-option 'imag)) (imagpart value)))

(define-widget color-option (QGLWidget color-triangle option)
  ()
  (:default-initargs
    :small NIL))

(define-widget small-color-option (QPushButton option)
  ()
  (:default-initargs
    :small T))

(define-subwidget (small-color-option dialog) (make-instance 'color-picker)
  (connect! dialog (input-updated) small-color-option (input-updated)))

(define-slot (small-color-option pressed) ()
  (declare (connected small-color-option (clicked)))
  (when (show dialog)
    (repaint small-color-option))
  (signal! small-color-option (input-done)))

(define-override (small-color-option paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter small-color-option)))
    (q+:fill-rect painter (q+:rect small-color-option) (value dialog))))

(defmethod value ((small-color-option small-color-option))
  (value (slot-value small-color-option 'dialog)))

(defmethod (setf value) (value (small-color-option small-color-option))
  (setf (value (slot-value small-color-option 'dialog)) value))

(define-widget symbol-option (QLineEdit option)
  ((case-conversion :initarg :case-conversion :accessor case-conversion))
  (:default-initargs
    :case-conversion (readtable-case *readtable*)
    :small T))

(define-initializer (symbol-option setup)
  (connect! symbol-option (editing-finished) symbol-option (input-done))
  (connect! symbol-option (text-changed string) symbol-option (input-updated))
  (call-next-method))

(defmethod value ((symbol-option symbol-option))
  (read-symbol (q+:text symbol-option) (case-conversion symbol-option)))

(defmethod (setf value) (value (symbol-option symbol-option))
  (setf (q+:text symbol-option) (format-symbol value (case-conversion symbol-option))))

(define-widget pathname-option (QWidget option)
  ((mode :initarg :mode :accessor mode)
   (filter :initarg :filter :accessor filter))
  (:default-initargs
    :mode :any
    :filter NIL
    :small T))

(define-subwidget (pathname-option text) (q+:make-qlineedit pathname-option)
  (setf (q+:size-policy text) (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.maximum)))
  (connect! text (editing-finished) pathname-option (input-done))
  (connect! text (text-changed string) pathname-option (input-updated)))

(define-subwidget (pathname-option button) (q+:make-qpushbutton "..." pathname-option)
  (setf (q+:maximum-width button) 30))

(define-subwidget (pathname-option layout) (q+:make-qhboxlayout pathname-option)
  (setf (q+:spacing layout) 0)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout text)
  (q+:add-widget layout button))

(define-slot (pathname-option show-dialog) ()
  (declare (connected button (clicked)))
  (with-finalizing ((dialog (q+:make-qfiledialog)))
    (setf (q+:directory dialog) (q+:text text))
    (setf (q+:file-mode dialog) (ecase mode
                                  ((T :any) (q+:qfiledialog.any-file))
                                  ((:existing) (q+:qfiledialog.existing-file))
                                  ((:directory :folder) (q+:qfiledialog.directory))))
    (when filter
      (setf (q+:name-filter dialog) filter))
    (when (find mode '(:directory :folder))
      (setf (q+:option dialog) (q+:qfiledialog.show-dirs-only)))
    (when (enum-equal (q+:exec dialog) (q+:qdialog.accepted))
      (setf (value pathname-option) (first (q+:selected-files dialog)))
      (signal! pathname-option (input-done)))))

(defmethod value ((pathname-option pathname-option))
  (uiop:parse-native-namestring (q+:text (slot-value pathname-option 'text))))

(defmethod (setf value) (value (pathname-option pathname-option))
  (setf (q+:text (slot-value pathname-option 'text))
        (uiop:native-namestring (or value *default-pathname-defaults*))))

(define-widget hash-table-option (QWidget option)
  ()
  (:default-initargs
    :small NIL))

(define-subwidget (hash-table-option layout) (q+:make-qformlayout hash-table-option))

(define-finalizer (hash-table-option teardown)
  (sweep-layout layout))

(defmethod value ((hash-table-option hash-table-option))
  (option-target-value hash-table-option))

(defmethod (setf value) (value (option hash-table-option))
  (teardown option)
  (loop for k being the hash-keys of value
        do (q+:add-row (slot-value option 'layout)
                       (prin1-to-string k) (make-auto-option value k :accessor-type :hash-table))))

(define-widget object-option (QWidget option)
  ()
  (:default-initargs
    :small NIL))

(define-subwidget (object-option layout) (q+:make-qformlayout object-option))

(define-finalizer (object-option teardown)
  (sweep-layout layout))

(defmethod value ((object-option object-option))
  (option-target-value object-option))

(defmethod (setf value) (value (option object-option))
  (teardown option)
  (loop for slot in (c2mop:class-slots (class-of value))
        for name = (c2mop:slot-definition-name slot)
        do (q+:add-row (slot-value option 'layout)
                       (format-symbol name) (make-auto-option value name :accessor-type :slot))))

(define-widget sequence-option (QWidget option)
  ()
  (:default-initargs
    :small NIL))

(define-subwidget (sequence-option layout) (q+:make-qvboxlayout sequence-option)
  (setf (q+:alignment layout) (q+:qt.align-top)))

(define-finalizer (sequence-option teardown)
  (sweep-layout layout))

(defmethod value ((sequence-option sequence-option))
  (option-target-value sequence-option))

(defmethod (setf value) (value (option sequence-option))
  (teardown option)
  (dotimes (i (length value))
    (q+:add-widget (slot-value option 'layout)
                   (make-auto-option value i :accessor-type :sequence))))

(defmethod make-auto-option (target reader &key writer (accessor-type :accessor) (updating :when-done))
  (let* ((value (generic-place-value target accessor-type reader))
         (option (make-instance (option-for-value value)
                                :target target :reader reader :writer writer
                                :accessor-type accessor-type :updating updating
                                :title (prin1-to-string value))))
    (if (option-small-p option)
        option
        (make-instance 'extern-option :option option))))

(define-widget extern-option (QWidget)
  ((option :initarg :option :reader option :finalized T)))

(define-subwidget (extern-option text) (q+:make-qlineedit extern-option)
  (setf (q+:size-policy text) (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.maximum)))
  (setf (q+:read-only text) T)
  (setf (q+:text text) (prin1-to-string (value option))))

(define-subwidget (extern-option button) (q+:make-qpushbutton "..." extern-option)
  (setf (q+:maximum-width button) 30))

(define-subwidget (extern-option layout) (q+:make-qhboxlayout extern-option)
  (setf (q+:spacing layout) 0)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout text)
  (q+:add-widget layout button))

(define-slot (extern-option show-dialog) ()
  (declare (connected button (clicked)))
  (setf (q+:window-flags option) (q+:qt.tool))
  (q+:show option))

(define-widget display-option (QLabel option)
  ()
  (:default-initargs
    :small T))

(defmethod value ((display-option display-option))
  (option-target-value display-option))

(defmethod (setf value) (value (display-option display-option))
  (setf (q+:text display-option) (prin1-to-string value)))

(defmethod option-for-value ((value character)) 'string-option)
(defmethod option-for-value ((value string)) 'string-option)
(defmethod option-for-value ((value integer)) 'integer-option)
(defmethod option-for-value ((value real)) 'small-double-option)
(defmethod option-for-value ((value complex)) 'complex-option)
(defmethod option-for-value ((value pathname)) 'pathname-option)
(defmethod option-for-value ((value symbol)) 'symbol-option)
(defmethod option-for-value ((value hash-table)) 'hash-table-option)
(defmethod option-for-value ((value sequence)) 'sequence-option)
(defmethod option-for-value ((value standard-object)) 'object-option)
(defmethod option-for-value ((value T)) 'display-option)

(defgeneric make-option (type &key &allow-other-keys)
  (:method ((type (eql 'boolean)) &rest args)
    (apply #'make-instance
           'boolean-option
           args))
  (:method ((type (eql 'string)) &rest args &key text)
    (apply #'make-instance
           (if text 'text-option 'string-option)
           args))
  (:method ((type (eql 'password)) &rest args)
    (apply #'make-instance
           'password-option
           args))
  (:method ((type (eql 'integer)) &rest args)
    (apply #'make-instance
           'integer-option
           args))
  (:method ((type (eql 'double)) &rest args &key small)
    (apply #'make-instance
           (if small 'small-double-option 'double-option)
           args))
  (:method ((type (eql 'complex)) &rest args)
    (apply #'make-instance
           'complex-option
           args))
  (:method ((type (eql 'pathname)) &rest args)
    (apply #'make-instance
           'pathname-option
           args))
  (:method ((type (eql 'symbol)) &rest args)
    (apply #'make-instance
           'symbol-option
           args))
  (:method ((type (eql 'hash-table)) &rest args)
    (apply #'make-instance
           'hash-table-option
           args))
  (:method ((type (eql 'sequence)) &rest args)
    (apply #'make-instance
           'sequence-option
           args))
  (:method ((type (eql 'standard-object)) &rest args)
    (apply #'make-instance
           'object-option
           args))
  (:method ((type (eql 'color)) &rest args &key small)
    (apply #'make-instance
           (if small 'small-color-option 'color-option)
           args)))

(define-widget option-container (QWidget listing)
  ()
  (:default-initargs
    :draggable NIL :sortable NIL :selectable NIL))

(defmethod coerce-item (item (option-container option-container))
  (make-instance 'option-container-item :item item :container option-container))

(defmethod item-acceptable-p ((option option) (option-container option-container))
  T)

(define-widget option-container-item (QWidget item-widget)
  ())

(define-subwidget (option-container-item title) (q+:make-qlabel option-container-item)
  (setf (q+:text title) (title (widget-item option-container-item))))

(define-subwidget (option-container-item layout) (q+:make-qgridlayout option-container-item)
  (setf (q+:margin layout) 2)
  (cond ((option-small-p (widget-item option-container-item))
         (setf (q+:column-stretch layout 0) 1)
         (q+:add-widget layout title 0 0 1 1)
         (q+:add-widget layout (widget-item option-container-item) 0 1 1 1))
        (T
         (q+:add-widget layout title 0 0 1 1)
         (q+:add-widget layout (widget-item option-container-item) 1 0 1 1))))

(defmethod widget-acceptable-p ((option-container-item option-container-item) (option-container option-container))
  T)

(defmacro create-options-for-object (object &body options)
  (let ((container (gensym "CONTAINER"))
        (target (gensym "TARGET")))
    (flet ((create-form (slot &rest args &key type &allow-other-keys)
             (let ((args (copy-list args)))
               (remf args :type)
               `(add-item (make-option ',type :target ,target :reader ',slot ,@args) ,container))))
      `(let ((,container (make-instance 'option-container))
             (,target ,object))
         ,@(loop for option in options
                 collect (apply #'create-form option))
         ,container))))
