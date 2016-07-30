#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(defpackage #:qtools-ui
  (:nicknames #:qui #:org.shirakumo.qtools.ui)
  (:use #:cl+qt)
  ;; cell.lisp
  (:export
   #:padding
   #:cell)
  ;; color-history.lisp
  (:export
   #:color-history
   #:color-count)
  ;; color-picker.lisp
  (:export
   #:color-picker)
  ;; color-sliders.lisp
  (:export
   #:rgb-color-slider
   #:hsv-color-slider)
  ;; color-triangle.lisp
  (:export
   #:color-triangle)
  ;; compass.lisp
  (:export
   #:compass)
  ;; configurable.lisp
  (:export
   #:configurable-class
   #:configurable-class-options
   #:configurable-class-option-order
   #:configurable-slot
   #:configurable-slot-option
   #:configurable
   #:configuration-container)
  ;; container.lisp
  (:export
   #:map-widgets
   #:map-items
   #:ensure-widget-order
   #:sorting
   #:do-widgets
   #:do-items
   #:container
   #:widgets
   #:sorted-container
   #:item-container
   #:sorted-item-container)
  ;; debugger.lisp
  (:export
   #:invoke-gui-debugger
   #:exit-with-restart
   #:debugger
   #:exit-restart)
  ;; dialog.lisp
  (:export
   #:dialog
   #:show
   #:simple-input-dialog
   #:show)
  ;; draggable.lisp
  (:export
   #:dragging
   #:draggable
   #:drag-start
   #:drag
   #:drag-end)
  ;; executable.lisp
  (:export
   #:executable
   #:process-executions
   #:execute
   #:execute-in-gui
   #:with-body-in-gui)
  ;; flow-layout.lisp
  (:export
   #:flow-layout)
  ;; input.lisp
  (:export
   #:input
   #:input-updated
   #:input-done
   #:value
   #:storing-input
   #:color-storing-input)
  ;; items.lisp
  (:export
   #:container
   #:widget-item
   #:item-widget
   #:coerce-item
   #:item-at
   #:item-position
   #:find-item
   #:add-item
   #:insert-item
   #:remove-item
   #:remove-item-at
   #:swap-items
   #:swap-items-at
   #:item-acceptable-p
   #:item<
   #:item=
   #:item<=
   #:item>=
   #:item-layout
   #:item-widget)
  ;; keychord-editor.lisp
  (:export
   #:keychord-editor)
  ;; layout.lisp
  (:export
   #:widget
   #:find-widget
   #:widget-position
   #:widget-at-point
   #:add-widget
   #:insert-widget
   #:remove-widget
   #:swap-widgets
   #:clear-layout
   #:update
   #:widget-acceptable-p
   #:layout)
  ;; listing.lisp
  (:export
   #:minimum-row-height
   #:fixed-row-height
   #:draggable
   #:listing
   #:listing-item)
  ;; mouse-propagator.lisp
  (:export
   #:target
   #:mouse-propagator)
  ;; options.lisp
  (:export
   #:option-effective-target
   #:option-target-value
   #:target
   #:reader
   #:writer
   #:title
   #:accessor-type
   #:option-updating
   #:option-small-p
   #:make-option
   #:option
   #:string-option
   #:text-option
   #:double-option
   #:small-double-option
   #:complex-option
   #:color-option
   #:small-color-option
   #:symbol-option
   #:pathname-option
   #:hash-table-option
   #:object-option
   #:sequence-option
   #:extern-option
   #:display-option
   #:option-container
   #:option-container-item
   #:create-options-for-object)
  ;; panel-container.lisp
  (:export
   #:orientation
   #:iconified-p
   #:iconify
   #:deiconify
   #:panel-container)
  ;; panel-main-window.lisp
  (:export
   #:panel-main-window)
  ;; panel.lisp
  (:export
   #:container
   #:title
   #:detachable-p
   #:collapsable-p
   #:titlebar-shown-p
   #:attached-p
   #:collapsed-p
   #:attach
   #:detach
   #:expand
   #:collapse
   #:exit
   #:panel)
  ;; repaintable.lisp
  (:export
   #:repaint
   #:repaintable)
  ;; repl.lisp
  (:export
   #:repl
   #:repl-eval
   #:repl-eval-loop
   #:repl-eval-inner
   #:repl-output-stream)
  ;; selectable.lisp
  (:export
   #:active-widget
   #:active-item
   #:selectable
   #:selectable-layout
   #:selectable-item)
  ;; slider.lisp
  (:export
   #:maximum
   #:minimum
   #:stepping
   #:default
   #:double-slider
   #:slider)
  ;; splitter.lisp
  (:export
   #:resize-widget
   #:orientation
   #:handle-size
   #:splitter
   #:splitter-handle)
  ;; toolkit.lisp
  (:export
   #:call-with-translation
   #:with-translation
   #:color-to-rgba
   #:rgba-to-color
   #:c
   #:coerce-color))
