#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-dialog
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Helper classes for constructing dialogs."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "dialog"))
  :depends-on (:qtools-ui-base
               :qtools-ui-helpers))
