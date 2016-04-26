#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(named-readtables:in-readtable :qtools)

(define-widget repl (QTextEdit)
  ((input-begin :initform 0 :accessor input-begin)
   (output-stream :initarg :output-stream :accessor output-stream)
   (error-stream :initarg :error-stream :accessor error-stream)
   (eval-thread :initform NIL :accessor eval-thread)
   (eval-lock :initform (bt:make-lock) :reader eval-lock)
   (state :initform :ready :accessor state)
   (print-queue :initform (make-array 0 :adjustable T :fill-pointer T) :reader print-queue)
   (print-lock :initform (bt:make-lock) :reader print-lock)))

(define-initializer (repl setup)
  (unless (slot-boundp repl 'output-stream)
    (setf output-stream (make-instance 'repl-output-stream :repl repl :color "orange")))
  (unless (slot-boundp repl 'error-stream)
    (setf error-stream (make-instance 'repl-output-stream :repl repl :color "red")))
  (setf (q+:minimum-size repl) (values 300 75))
  (setf (q+:window-title repl) "REPL")
  (repl-output-prefix repl *package*)
  ;; in order to have a persistent set of bindings, we use a thread.
  (setf eval-thread (bt:make-thread (lambda () (repl-eval-loop repl))
                                    :initial-bindings
                                    `((*terminal-io* . ,output-stream)
                                      (*standard-input* . ,*standard-input*)
                                      (*standard-output* . ,output-stream)
                                      (*error-output* . ,error-stream)
                                      (*query-io* . ,output-stream)
                                      (*trace-output* . ,output-stream)
                                      (*debug-io* . ,output-stream)
                                      (/ . NIL) (// . NIL) (/// . NIL)
                                      (* . NIL) (** . NIL) (*** . NIL)
                                      (+ . NIL) (++ . NIL) (+++ . NIL)))))

(define-finalizer (repl teardown)
  (setf (state repl) :finalizing)
  (loop for i from 0
        while (bt:thread-alive-p (eval-thread repl))
        do (sleep 0.01)
           (when (< 10 i)
             #+verbose (v:warn :qui.repl "REPL loop did not exit gracefully.")
             (bt:destroy-thread (eval-thread repl))
             (return))))

(define-subwidget (repl font) (q+:make-qfont "Monospace" 10)
  (setf (q+:style-hint font) (q+:qfont.type-writer))
  (setf (q+:font repl) font))

(define-subwidget (repl palette) (q+:make-qpalette)
  (let ((background (q+:color palette (q+:qpalette.window)))
        (foreground (q+:color palette (q+:qpalette.text))))
    (setf (q+:color palette (q+:qpalette.base)) background)
    (setf (q+:color palette (q+:qpalette.text)) foreground)
    (setf (q+:palette repl) palette)))

(define-signal (repl return-pressed) ())
(define-signal (repl process-print-queue) ())

(define-override (repl key-press-event) (ev)
  (cond ;; Signal return pressed.
    ((or (= (q+:key ev) (q+:qt.key_enter))
         (= (q+:key ev) (q+:qt.key_return)))
     (call-next-qmethod)
     (if (enum-equal (q+:modifiers ev)
                     (q+:qt.control-modifier))
         (q+:insert-plain-text repl (format NIL "~%"))
         (signal! repl (return-pressed))))
    ;; Catch repl-escape to forbid removing text before input.
    ((= (q+:key ev) (q+:qt.key_backspace))
     (when (< (input-begin repl) (repl-cursor repl))
       (call-next-qmethod)))
    ;; Delegate standard.
    (T
     (call-next-qmethod))))

(define-slot (repl eval) ()
  (declare (connected repl (return-pressed)))
  (destructuring-bind (type data package)
      (repl-eval repl (repl-input repl))
    (let ((*package* package))
      (case type
        (:success (repl-output-values repl data))
        (:failure (repl-output-error repl data))))
    (repl-output-prefix repl package)))

(define-slot (repl process-print-queue) ()
  (declare (connected repl (process-print-queue)))
  (bt:with-lock-held ((print-lock repl))
    (loop for string across print-queue 
          do (q+:move-cursor repl (q+:qtextcursor.end))
             (q+:insert-html repl string)
             (q+:move-cursor repl (q+:qtextcursor.end))
          finally (setf (fill-pointer print-queue) 0))))

(defmethod repl-eval ((repl repl) form)
  (loop for state = (state repl)
        until (or (eql state :finalizing)
                  (when (eql state :ready)
                    (bt:with-lock-held ((eval-lock repl))
                      (setf (state repl) (list* :eval form)))))
        do (sleep 0.01))
  (loop for state = (state repl)
        until (or (eql state :finalizing)
                  (when (and (listp state) (find (car state) '(:success :failure)))
                    (setf (state repl) :ready)
                    (return state)))
        do (sleep 0.01)))

(defmethod repl-eval-loop ((repl repl))
  (loop for state = (state repl)
        until (eql state :finalizing)
        do (when (and (listp state) (eql (car state) :eval))
             (bt:with-lock-held ((eval-lock repl))
               (setf (state repl)
                     (handler-case
                         (list :success (repl-eval-inner repl (read-from-string (cdr state))) *package*)
                       (error (err)
                         (list :failure err *package*))))))
           (sleep 0.01)))

(defmethod repl-eval-inner :around ((repl repl) form)
  (let ((values (call-next-method)))
    (shiftf /// // / values)
    (shiftf *** ** * (first values))
    (shiftf +++ ++ + form)
    values))

(defmethod repl-eval-innner ((repl repl) form)
  (multiple-value-list (eval form)))

(defun repl-cursor (repl)
  (q+:position (q+:text-cursor repl)))

(defun repl-output (repl format-string &rest args)
  (let ((string (apply #'format NIL format-string args)))
    (bt:with-lock-held ((print-lock repl))
      (vector-push-extend string (print-queue repl)))
    (signal! repl (process-print-queue))))

(defun repl-escape (text)
  (flet ((r (text find replace)
           (cl-ppcre:regex-replace-all find text replace)))
    (r (r (r (r text "&" "&amp;") "<" "&lt;") ">" "&gt;") "\\n" "<br />")))

(defun repl-output-line (repl)
  (repl-output repl "<br />"))

(defun repl-output-form (repl form)
  (repl-output repl "~a<br />" (repl-escape (write-to-string form))))

(defun repl-output-colored (repl color format-string &rest args)
  (repl-output repl "<span style=\"color:~a;\">~a</span>" color (apply #'format NIL format-string args)))

(defun repl-output-prefix (repl package)
  (repl-output-colored repl "red" "~@[~a~]&gt;" (shortest-package-name package))
  (repl-output repl "&nbsp;")
  (setf (input-begin repl) (repl-cursor repl)))

(defun repl-output-comment (repl format-string &rest args)
  (repl-output-colored repl "gray" "; ~a<br />" (apply #'format NIL format-string args)))

(defun repl-output-values (repl values)
  (if values
      (dolist (value values)
        (repl-output-colored repl "cyan" "~a<br />" (repl-escape (write-to-string value))))
      (repl-output-comment repl "No values.")))

(defun repl-output-error (repl error)
  (repl-output-comment repl "<span style=\"color:red;\">Error:</span> ~a" (repl-escape (princ-to-string error)))
  (repl-output-comment repl "[Condition of type ~a]" (repl-escape (princ-to-string (type-of error)))))

(defun repl-input (repl)
  (assert (< (input-begin repl) (repl-cursor repl))
          () "No input at this point.")
  (subseq (q+:to-plain-text repl) (input-begin repl) (repl-cursor repl)))

(defclass repl-output-stream (trivial-gray-streams:fundamental-character-output-stream
                              trivial-gray-streams:trivial-gray-stream-mixin)
  ((repl :initarg :repl :accessor repl)
   (buffer :initform (make-string-output-stream) :accessor buffer)
   (color :initarg :color :accessor color))
  (:default-initargs
   :repl (error "CONSOLE required.")
   :color "orange"))

(defmethod trivial-gray-streams:stream-clear-output ((stream repl-output-stream))
  (setf (buffer stream) (make-string-output-stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream repl-output-stream))
  (let ((string (get-output-stream-string (buffer stream))))
    (repl-output-colored (repl stream) (color stream) "~a" (repl-escape string)))
  (trivial-gray-streams:stream-clear-output stream))

(defmethod trivial-gray-streams:stream-force-output ((stream repl-output-stream))
  (trivial-gray-streams:stream-finish-output stream))

(defmethod trivial-gray-streams:stream-write-string ((stream repl-output-stream) string &optional (start 0) end)
  (write-string string (buffer stream) :start start :end end)
  (trivial-gray-streams:stream-finish-output stream))

(defmethod trivial-gray-streams:stream-write-char ((stream repl-output-stream) char)
  (write-string (string char) stream))

(defmethod trivial-gray-streams:stream-terpri ((stream repl-output-stream))
  (write-char #\Newline stream))
