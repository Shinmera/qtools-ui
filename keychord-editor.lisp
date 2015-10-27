#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget keychord-editor (QDialog)
  ((old-accelerator :initform "")
   (keychord-class :initarg :class :initform NIL)))

(define-subwidget (keychord-editor action-table) (q+:make-qtablewidget (length (widget-actions keychord-editor)) 2)
  (connect! action-table (current-changed int int) keychord-editor (record-action int int))
  (connect! action-table (value-changed int int) keychord-editor (validate-action int int))
  
  (setf (q+:horizontal-header-item action-table 0) (q+:make-qtablewidgetitem "Item Name"))
  (setf (q+:horizontal-header-item action-table 1) (q+:make-qtablewidgetitem "Keychord"))
  (setf (q+:visible (q+:vertical-header action-table)) NIL)
  (setf (q+:resize-mode (q+:horizontal-header action-table)) (q+:qheaderview.stretch))
  (loop for row from 0
        for action in (widget-actions keychord-editor)
        for label = (q+:make-qtablewidgetitem (q+:text action))
        for accel = (q+:make-qtablewidgetitem (q+:to-string (q+:shortcut action)))
        do (setf (q+:flags label) 0)
           (setf (q+:item action-table row 0) label)
           (setf (q+:item action-table row 1) accel)))

(define-subwidget (keychord-editor ok-button) (q+:make-qpushbutton "&Ok")
  (connect! ok-button (clicked) keychord-editor (accept)))

(define-subwidget (keychord-editor cancel-button) (q+:make-qpushbutton "&Cancel")
  (connect! cancel-button (clicked) keychord-editor (reject)))

(define-subwidget (keychord-editor button-layout) (q+:make-qhboxlayout)
  (setf (q+:spacing button-layout) 8)
  (q+:add-stretch button-layout 8)
  (q+:add-widget button-layout ok-button)
  (q+:add-widget button-layout cancel-button))

(define-subwidget (keychord-editor layout) (q+:make-qvboxlayout keychord-editor)
  (setf (q+:margin layout) 8)
  (setf (q+:spacing layout) 8)
  (q+:add-widget layout action-table)
  (q+:add-layout layout button-layout))

(define-override (keychord-editor accept) ()
  (loop for row from 0
        for action in (widget-actions keychord-editor)
        do (setf (q+:shortcut action) (q+:make-qkeysequence (q+:text (q+:item action-table row 1)))))
  (stop-overriding))

(define-slot (keychord-editor record-action) ((row int) (column int))
  (setf old-accelerator (q+:text (q+:item action-table row column))))

(define-slot (keychord-editor validate-action) ((row int) (column int))
  (let* ((item (q+:item action-table row column))
         (text (q+:to-string (q+:make-qkeysequence (q+:text item)))))
    (if (and (q+:is-empty text)
             (not (q+:is-empty (q+:text item))))
        (setf (q+:text item) old-accelerator)
        (setf (q+:text item) text))))
