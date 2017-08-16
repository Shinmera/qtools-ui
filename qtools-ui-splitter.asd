#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-splitter
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A better version of the QSplitter, allowing a dynamic size based on the components' size."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "splitter"))
  :depends-on (:qtools-ui-base
               :qtools-ui-helpers
               :qtools-ui-container))
