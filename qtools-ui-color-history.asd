#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-color-history
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple history for colors."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "color-history"))
  :depends-on (:qtools-ui-base
               :qtools-ui-helpers
               :qtools-ui-flow-layout))
