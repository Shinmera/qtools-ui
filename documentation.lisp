#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

;; bytearray.lisp
(docs:define-docs
  (cl:function from-byte-array
    "Converts from a QByteArray into a (vector (unsigned-byte 8)).

The optional argument FINALIZE states whether the QByteArray should be finalized
after the call completes.")
  (cl:function to-byte-array
    "Converts from a (vector (unsigned-byte 8)) into a QByteArray."))

;; cell.lisp
(docs:define-docs
  (cl:function padding
    "Padding in the number of pixels to use between the cell edge and the item.")

  (type cell
    "A cell is a generic item container that is selectable and draggable."))

;; color-history.lisp
(docs:define-docs
  (type color-history
    "A widget allowing access to N most recently accessed colors.

The number of colors stored in the color history can be set during widget
creation via the keyword argument :COLOR-COUNT.")

  (cl:function color-count
    "Returns the color count of the given color history widget."))

;; color-picker.lisp
(docs:define-docs
  (type color-picker
    "A widget allowing the user to choose a color via its RGB or HSV values."))

;; color-sliders.lisp
(docs:define-docs
  (type rgb-color-slider
    "A widget for an RGB sliders color chooser.")

  (type hsv-color-slider
    "A widget for an HSV sliders color chooser."))

;; color-triangle.lisp
(docs:define-docs
  (type color-triangle
    "A widget for an HSV colour wheel triangle as often used in graphics applications."))

;; compass.lisp
(docs:define-docs
  (type compass
    "A layout widget that aligns items North, East, South, West, and Center.
Use the corresponding keywords for the widget places."))

;; TODO configurable.lisp

;; container.lisp
(docs:define-docs
  (cl:function map-widgets
    "Map the function over the container's widgets.
The mapping order is in sequence with the widget positions.")

  (cl:function map-items
    "Map the function over the container's items.
The mapping order is in sequence with the item positions.")

  (cl:function ensure-widget-order
    "Make sure that the widgets of the container are in the right order.
This may change the widget's positions.")

  (cl:function sorting
    "The function used for sorting the container. If NIL, no sorting is applied.
The function must accept two arguments to compare.")

  (cl:function widgets
    "The direct widgets stored in the container.
You should not used this unless you are implementing a container yourself, or know
what you are doing, as the data structure is not necessarily safe to use directly,
or even specified to be of a certain type.")

  (cl:function do-widgets
    "Loop over the container's widgets.

See MAP-WIDGETS")

  (cl:function do-items
    "Loop over the container's items.

See MAP-ITEMS")

  (type container
    "A simple container to hold a list of widgets and manage the various layout functions.
This does not actually concern itself with arranging the widgets and only handles
the internal representation of the widgets. As such, this class is to be used as
a superclass for an actual layout that then takes care of the widget arrangement.")

  (type sorted-container
    "A container that also supports automatic sorting of the widgets.

See CONTAINER
See SORTING")

  (type item-container
    "A container that uses item containers instead of direct widgets.

See CONTAINER
See ITEM-LAYOUT")

  (type sorted-item-container
    "An item-container that also supports automatic sorting of the items.

See ITEM-CONTAINER
See SORTED-CONTAINER"))

;; debugger.lisp
(docs:define-docs
  (cl:function invoke-gui-debugger
    "Invokes the Qtools debugger with the provided condition.")
  (cl:function exit-with-restart
    "Returns the restart selected in the debugger widget after it closes.")
  (type debugger
    "A Lisp debugger widget implemented in Qtools.")
  (cl:function exit-restart
    "Accesses the debugger widget's exit restart slot."))

#+(or)
;; dialog.lisp TODO
(docs:define-docs
  (type dialog
    "")
  (cl:function show
    "")
  (type simple-input-dialog
    ""))

;; drag-and-drop.lisp
(docs:define-docs
  (variable *mime-data-with-object-type*
    "The default value for MIME types used by draggable objects.")

  (type mime-data-with-object
    "Subclass of QMimeData capable of holding Lisp objects.

This class is a direct subclass of QMimeData with two modifications:

  * It contains a slot called OBJECT that can be used to transmit arbitrary Lisp
    data via drag and drop.
  * The other is the :MIME-TYPE constructor keyword. This keyword can be used to
    provide the MIME type of the dragged data.

Class MIME-DATA-WITH-OBJECT is not meant to be instantiated directly by the user
as it is a part of this drag and drop framework. Instead, the user should
subclass DRAGGABLE.

See DRAGGABLE
See DROP-TARGET")

  (type draggable
    "Superclass of all objects that are draggable.

This class should be subclassed by all widgets that are meant to be draggable.
Mouse-clicking an instance of this class will initiate drag behaviour.

Each DRAGGABLE has a MIME-TYPE slot, describing the MIME type of the content that is
being dragged around. Its value must be a string and defaults to the value of
*MIME-DATA-WITH-OBJECT-TYPE*.

This class overrides MOUSE-PRESS-EVENT.

See MIME-DATA-WITH-OBJECT
See DROP-TARGET
See DROP-ACCEPTABLE-P
See DROP")

  (type drop-target
    "Superclass of all objects that accept drops.

This class should be subclassed by all widgets that are meant to accept drops. Dropping a
drag over this widget will initiate drop behaviour.

Each DROP-TARGET has a MIME-TYPE slot, describing the MIME type of the content that is
acceptable for dropping. Its value must be a string and defaults to the value of
*MIME-DATA-WITH-OBJECT-TYPE*.

This class overrides DRAG-ENTER-EVENT and DROP-EVENT.

See MIME-DATA-WITH-OBJECT
See DRAGGABLE
See DROP-ACCEPTABLE-P
See DROP")

  (cl:function drop-acceptable-p
    "Whether it is possible to drop ITEM onto TARGET.

Syntax: (drop-acceptable-p item target)

To make it possible to drop item of class A onto an object of class B, define a method
\(defmethod drop-acceptable-p ((item A) (target B)) T). Keep in mind that the MIME types of
the DRAGGABLE and DROP-TARGET in question must match, even if such a method is defined on
both classes. (This is why we use a single MIME type for everything - to move the
drag-and-drop dispatch to the Common Lisp GF mechanism.)

This generic function has a default method that returns NIL.

A method is provided for MIME-DATA-WITH-OBJECT that returns T.

See MIME-DATA-WITH-OBJECT
See DRAGGABLE
See DROP-TARGET
See DROP")

  (cl:function drop
    "Implements logic to be run after an item is dropped.

Syntax: (drop item target)

This generic function implements the consequences of dropping ITEM onto TARGET.

No default methods are provided for this method.

See MIME-DATA-WITH-OBJECT
See DRAGGABLE
See DROP-TARGET
See DROP-ACCEPTABLE-P"))

;; TODO add "See ..." at the end of docstrings where applicable
;; draggable.lisp
(docs:define-docs
  (cl:function dragging
    "Whether the draggable is currently being dragged.")

  (cl:function drag-start
    "Called whenever the draggable is beginning to be dragged.
This usually happens during a mouse-press event.")

  (cl:function drag
    "Called whenever the draggable is being dragged around.
This usually happens during a mouse-move event.")

  (cl:function drag-end
    "Called whenever the draggable has stopped being dragged.
This usually happens during a mouse-release event.")

  (type draggable
    "A helper class to be used when you need to support dragging of your widget.

See DRAGGING
See DRAG-START
See DRAG
See DRAG-END"))

;; executable.lisp
(docs:define-docs
  (type executable
    "A qobject superclass that allows running functions within the GUI thread.

See EXECUTE
See EXECUTE-IN-GUI
See WITH-BODY-IN-GUI")

  (cl:function execute
    "Performs the execution of an object.

An ABORT restart is always available during the execution of this method.

A standard method for FUNCTION objects exists.")

  (cl:function execute-in-gui
    "Schedules the execution to be executed within the executable's GUI thread.

When exactly the execution happens cannot be predetermined. However, it should
be approximately whenever the Qt event loop processes its next batch of events.

See EXECUTABLE
See EXECUTE")

  (cl:function with-body-in-gui
    "Convenience wrapper macro around EXECUTE-IN-GUI

See EXECUTE-IN-GUI"))

;; items.lisp
(docs:define-docs
  (cl:function container
    "The container of the item-widget.

See ITEM-WIDGET")

  (cl:function widget-item
    "The actual item wrapped by the item-widget.

See ITEM-WIDGET")

  (cl:function item-widget
    "Find the item-widget for the given item in the layout.")

  (cl:function coerce-item
    "Create a suitable item-widget for the item.")

  (cl:function item-at
    "Return the item at the specified place in the layout.")

  (cl:function item-position
    "Return the position of the item in the layout.

See WIDGET-POSITION")

  (cl:function find-item
    "Find the item in the layout.

See FIND-WIDGET")

  (cl:function add-item
    "Add the item to the layout.

See ADD-WIDGET
See COERCE-ITEM")

  (cl:function insert-item
    "Insert the item into the layout at the specified place.

See INSERT-WIDGET
See COERCE-ITEM")

  (cl:function remove-item
    "Remove the item from the layout.

See REMOVE-WIDGET
See ITEM-WIDGET")

  (cl:function remove-item-at
    "Remove the item at the specified place in the layout.

See REMOVE-WIDGET")

  (cl:function swap-items
    "Swap the two items in their place in the layout.
Note that the implementation might swap the corresponding item-widget,
or it may also choose to swap the items in place. As such, the item-widget's
positions may change, or the actual item-widget of the item may change.")

  (cl:function swap-items-at
    "Swap the two items at the specified places in the layout.

See SWAP-ITEMS")

  (cl:function item-acceptable-p
    "A predicate to decide whether the item is suitable for inclusion in the widget.")

  (cl:function item<
    "Whether A precedes B.
Default methods for STRING and NUMBER exist, as well as a general method that simply
prints the object to a string using PRINC and calls ITEM< again with the results of that.

Add your own methods to this if you need more precise sorting.")

  (cl:function item=
    "Whether A is equal to B.
Default methods for STRING and NUMBER exist, as well as a general method that simply
prints the object to a string using PRINC and calls ITEM= again with the results of that.

Add your own methods to this if you need more precise sorting.")

  (cl:function item>
    "Whether A follows B.
Uses ITEM< and ITEM= to calculate the result. You should not need to add methods to this.")

  (cl:function item<=
    "Whether A precedes B.
Uses ITEM< and ITEM= to calculate the result. You should not need to add methods to this.")

  (cl:function item>=
    "Whether A follows B.
Uses ITEM< and ITEM= to calculate the result. You should not need to add methods to this.")

  (type item-layout
    "A layout to contain items.

See LAYOUT")

  (type item-widget
    "A widget to contain an item.
Depending on the item type, an item may or may not be contained in multiple item-widgets
at the same time. However, an item-widget itself can only be contained once and only in
one layout at a time."))

;; keychord-editor.lisp
(docs:define-docs
  (type keychord-editor
    "A simple dialog to allow dynamic changing of keychords as defined in a menu definition.

See QTOOLS:DEFINE-MENU"))

;; layout.lisp
(docs:define-docs
  (cl:function widget
    "Returns the widget at the specified place in the layout.")

  (cl:function find-widget
    "Find the widget in the layout.

See FIND")

  (cl:function widget-position
    "Find the position of the widget in the layout.

See POSITION")

  (cl:function widget-at-point
    "See if there is a widget in the layout at the point and return it if it exists.
POINT can be a cons of X and Y or a QPoint.
The coordinates have to be relative to the layout.")

  (cl:function add-widget
    "Add the widget to the layout.
The positioning of the widget is completely up to the layout.")

  (cl:function insert-widget
    "Insert the widget at the specified place in the layout.")

  (cl:function remove-widget
    "Remove the widget or the widget at the specified place from the layout.")

  (cl:function swap-widgets
    "Swap the two widgets or the widgets at the specified places in the layout.")

  (cl:function clear-layout
    "Clear all widgets from the layout.")

  (cl:function update
    "Update the layout widgets' geometry.

This is automatically called if the layout receives a layout-request event or is resized.
It's also automatically called on the various layout modifying operations.
If you add new operations that modify the layout as well without calling the preexisting
ones, you should call this method to ensure the widgets are restored as appropraite.

If you subclass a layout, you should implement a method on this to calculate yout layouts
widgets' geometry properly.")

  (cl:function widget-acceptable-p
    "Predicate to determine whether the layout accepts the given widget.
This test is automatically called on all the predefined widget adding functions to make
sure no bad widgets can be inserted into a layout.

You should add methods to this to either further restrict or permit further widgets.
You should call this to check for permission if you add new layout manipulating functions
that don't call out to the standard layout functions.")

  (type layout
    "Superclass for all custom layout widgets."))

;; listing.lisp
(docs:define-docs
  (cl:function minimum-row-height
    "Accessor to the minimum row height of the listing.")

  (cl:function fixed-row-height
    "Accessor to the fixed row height of the listing.
If NIL, the row heights are dynamic, otherwise the listing will enforce this height.")

  (cl:function draggable
    "Accessor to whether widgets are draggable or not.")

  (type listing
    "Replacement for the QListWidget.
Allows sorting, dragging, selecting, and of course dynamic modification of the contents.
Unlike the QListWidget, this allows adding actual widgets, not just strings.")

  (type listing-item
    "The item container for the listing container."))

;; mouse-propagator.lisp
(docs:define-docs
  (cl:function target
    "The target to send the propagated mouse events to.")

  (type mouse-propagator
    "A helper class that propagates mouse events somewhere else, by default to itself.
This implements an event filter. To catch the events, use QObject::installEventFilter."))

;; panel-container.lisp
(docs:define-docs
  (cl:function iconified-p
    "Accessor to whether the panel-container is iconified or not.
If iconified, the panels are not actually shown, only their titles or icons.")

  (cl:function iconify
    "Iconify the panel-container.

See ICONFIFIED-P")

  (cl:function deiconify
    "Deiconify the panel-container.

See ICONFIFIED-P")

  (type panel-container
    "A container for panels.
Supports iconifying, rearranging of the panels, and vertical or horizontal orientation."))

;; panel.lisp
(docs:define-docs
  (cl:function title
    "The title displayed for the panel.")

  (cl:function detachable-p
    "Accessor to whether the panel is detachable.")

  (cl:function collapsable-p
    "Accessor to whether the panel is collapsable.")

  (cl:function titlebar-shown-p
    "Accessor to whether the panel's titlebar is visible.")

  (cl:function attached-p
    "Accessor to whether the panel is attached.

See ATTACH
See DETACH")

  (cl:function collapsed-p
    "Accessor to whether the panel is collapsed.

See COLLAPSE
See EXPAND")

  (cl:function attach
    "Tell the panel to attach itself to a container.
If NIL is passed as the container, the panel will try to use the last container it has been
attached to to attach to again. If no container is given or no previous container exists,
an error is signalled. Panels can only be attached to one container at a time.")

  (cl:function detach
    "Tell the panel to detach itself from a container.
If it is not currently attached to anything, an error is signalled. The panel will remember
the container it has been attached to so it can easily be reattached later.")

  (cl:function expand
    "Make the panel's center widget visible.")

  (cl:function collapse
    "Make the panel's center widget invisible.")

  (cl:function exit
    "Close the panel.")

  (type panel
    "A dockable and collapsible panel to contain a widget.
Useful in situations where you want to build a UI that contains several parts that the user
should be able to freely arrange to their liking."))

;; repaintable.lisp
(docs:define-docs
  (cl:function repaint
    "Cause the repaintable to be repainted.
Unlike QWidget::repaint, this method is safe to be called from any thread as it will use a
signal to reach the main thread in which drawing events are permitted.")

  (type repaintable
    "A widget that is safely repaintable by offering a method and signal to cause a repaint in the main thread.

See REPAINT"))

;; selectable.lisp
(docs:define-docs
  (cl:function active-widget
    "Accessor for the currently active widget on the layout.")

  (cl:function active-item
    "Accessor for the currently active item on the layout.")

  (cl:function selectable
    "Accessor for whether selecting a widget/item is allowed.")

  (type selectable-layout
    "An item-layout that allows selecting a certain item.")

  (type selectable-item
    "An item that permits being selected."))

;; slider.lisp
(docs:define-docs
  (cl:function maximum
    "Accessor for the maximum of the slider.")

  (cl:function minimum
    "Accessor for the minimum of the slider.")

  (cl:function stepping
    "Accessor for the step size of the slider.")

  (cl:function default
    "Accessor for the slider's default value, if any.")

  (type double-slider
    "Qt does not provide a floating-point slider, hence this.
It uses a standard slider, but uses an internal divisor to achieve floating point values.

See MAXIMUM
See MINIMUM
See STEPPING")

  (type slider
    "A neat slider widget that combines a double-slider, a spin box, and a potential defaulting button.
If the default is NIL, no button is displayed.

See MAXIMUM
See MINIMUM
See STEPPING
See DEFAULT"))

;; splitter.lisp
(docs:define-docs
  (cl:function resize-widget
    "Resize the layout's widget to the given size.")

  (cl:function orientation
    "Accessor for the layout's orientation, must be one of :vertical or :horizontal.")

  (cl:function handle-size
    "Accessor for the size of a splitter's handle.")

  (type splitter
    "Similar to QSplitter, but instead of distributing all available space, only uses up and extends to as much space as is needed by its widgets."))

;; svgtools.lisp
(docs:define-docs
  (cl:function svg-pixmap
    "Draws the provided SVG file on a new pixmap with the provided scale and returns the pixmap.

The SCALE keyword argument may be a positive integer or float. The SVG's
original sizes will be multiplied by that factor and rounded to the nearest
integer.
The RENDERER keyword argument may be provided in case multiple SVG are rendered
one after another. This should avoid the cost of instantiating new QSvgRenderers
in succession."))

;; toolkit.lisp
(docs:define-docs
  (cl:function call-with-translation
    "Call the function while translating the painter by the target.

See QPainter::translate")

  (cl:function with-translation
    "Convenience macro around call-with-translation

See CALL-WITH-TRANSLATION")

  (cl:function color-to-rgba
    "Turns an rgba quadruplet into an integer.")

  (cl:function rgba-to-color
    "Turns an integer into an rgba quadruplet.")

  (cl:function c
    "Returns a corresponding QColor object.
Note that these objects are cached. You should never modify them.")

  (cl:function coerce-color
    "Coerce the color into a QColor object.
Can be either a QColor, a list of the R G B and A components, or an RGBA
integer. If not a direct QColor, the value is resolved as per C.

See RGBA-TO-COLOR
See C")

  (cl:function hue-shift
    "Destructively shifts the hue of target image by the provided amount of degrees.
IMAGE must be a ARGB32-format QImage and rotation must be a single-float between
0.0 (inclusive) and 360.0 (exclusive)."))
