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
  ;; color-triangle.lisp
  (:export
   #:color-triangle)
  ;; compass.lisp
  (:export
   #:compass)
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
  ;; draggable.lisp
  (:export
   #:dragging
   #:draggable
   #:drag-start
   #:drag
   #:drag-end)
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
  ;; panel-container.lisp
  (:export
   #:orientation
   #:iconified-p
   #:iconify
   #:deiconify
   #:panel-container)
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
  ;; selectable.lisp
  (:export
   #:active-widget
   #:active-item
   #:selectable
   #:selectable-layout)
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
   #:splitter)
  ;; toolkit.lisp
  (:export
   #:call-with-translation
   #:with-translation
   #:color-to-rgba
   #:rgba-to-color
   #:c
   #:coerce-color))
