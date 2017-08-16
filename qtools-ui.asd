#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui
  :version "0.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of components and utilities for use in Qt applications."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :depends-on (:qtools-ui-base
               :qtools-ui-cell
               :qtools-ui-color-history
               :qtools-ui-color-picker
               :qtools-ui-color-sliders
               :qtools-ui-color-triangle
               :qtools-ui-compass
               :qtools-ui-container
               :qtools-ui-debugger
               :qtools-ui-dialog
               :qtools-ui-flow-layout
               :qtools-ui-helpers
               :qtools-ui-keychord-editor
               :qtools-ui-layout
               :qtools-ui-listing
               :qtools-ui-options
               :qtools-ui-panels
               :qtools-ui-plot
               :qtools-ui-repl
               :qtools-ui-slider
               :qtools-ui-splitter
               :qtools-ui-notification))
