## About Qtools-UI
This is a collection of useful widgets and pre-made components for use in your Qt applications. There is no specific scope that limits what might fit into this library, but for the most part it concerns itself with things like layouting and other base components that can be used anywhere.

## Why Another Library?
This library builds on [Qtools](https://shinmera.github.io/qtools). However, while Qtools aims to provide a useful abstraction layer on top of CommonQt to make programming easier, it shouldn't really concern itself with providing useful standard components or replacement components for what Qt provides. That's where this comes in.

Unfortunately for us, Qt is rather lacklustre in certain departments. Some standard components are either very scarcely actually useful, or close to broken in their behaviour. Most annoying of all, the layouts in Qt are usually not designed with the idea of being rewriteable, meaning it's sometimes a real pain if you would just like to remove or shuffle the widgets around.

So, in an attempt to fix and circumvent this all, this library will contain a lot of reinventions, but does so in a much more reusable and extensible manner, allowing you to use parts of it and build on them as well.

## Extent and Contribution
There is no set constraint on the extent of what this library might contain in terms of pre-made components. As such, suggestions for further components are very welcome. If you wrote a widget that might be generally useful even outside of your specific application, a pull-request to add it to this would be much appreciated.

Qtools-UI offers individual ASDF systems for almost every component, meaning that if you do end up using it, you won't have to carry all the parts with you that you don't need. As such, the growth of this library is not impaired in any manner aside from the potentially growing complexity that should be minimised where possible.

## How To
Before using this library, you should get yourself familiar with writing Qt applications in general, hopefully by using [Qtools](https://shinmera.github.io/qtools). Next, you'll simply need to add `qtools-ui` or a more specific component system to your dependencies. All components can then be instantiated through `make-instance` and the respective symbol from the `qui` package. All components are put into the same package, even if they are not necessarily loaded all at once.

How to use the individual components of course varies by what it is. For this, hit up the documentation string for whichever you need. A very primitive example of some nifty components follows:

    (cl+qt:with-main-window (w (make-instance 'qui:panel-container))
      (qui:add-widget (make-instance 'qui:panel :title "An empty panel. :(") w)
      (qui:add-widget (make-instance 'qui:panel :title "A slider, whoa!"
                                                :center (make-instance 'qui:slider)) w))

## General Layouting Concepts
Probably the most important aspect of Qtools-UI is the layouting mechanism. Central to this are the following basic functions: `widget`, `find-widget`, `widget-position`, `widget-at-point`, `add-widget`, `insert-widget`, `remove-widget`, `swap-widgets`, `clear-layout`. These allow you to manipulate and inspect the layout contents easily. Internally, the management of the widgets is up to the specific layout at hand. The actual positioning of the widgets is done in the layout's `update` mechanism, which therefore has to be called whenever a change occurs that might influence the widgets' positions. As a user of layouts you should not have to touch this method. Layouts also allow controlling which widgets they may contain, to ensure that no unsuitable widgets can enter the layout and screw up the management. This policy is controlled by `widget-acceptable-p`.

Built on top of the layout system is the items system, which very closely mimics the layout API. Items are meant for situations where the actual widget in a layout has to be a wrapper around the actual widget or item the user provides. This is useful in contexts like the `listing`, which needs to allow changing the colour when a row is selected and so forth. In order to not have to intrude into the user supplied widgets and to provide a more general system for this kind of tasks, the items are there for you. As a user you should however be aware of whether the component you're using is relying on widgets, or has to use the items layer, since you will still be able to access and manipulate the widgets directly, which might then lead to unexpected behaviour. For the most part though, the items functions map rather directly to the widgets ones, plus or minus a `widget-item` or `item-widget` call. Similarly to `widget-acceptable-p`, the items system has a `item-accceptable-p` method to control which items the item-layout can handle.

In order to allow sorting, items also allow a general sorting mechanism that is facilitated through the `item<`, `item=`, `item>`, `item<=`, and `item>=` functions. If you provide a widget as an item to a sorted layout, you likely will want to add methods to at least `item<` and `item=` to perform the proper ordering test.
