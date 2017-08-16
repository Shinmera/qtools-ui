#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-panels
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A panelling system that allows floating, docking, and collapsing."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "panel")
               (:file "panel-container")
               (:file "panel-main-window"))
  :depends-on (:qtools-ui-base
               :qtools-ui-helpers
               :qtools-ui-compass
               :qtools-ui-splitter))
