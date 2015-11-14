#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-ui-flow-layout
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A flow-layout that lets widgets flow to the next line."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "flow-layout"))
  :depends-on (:qtools-ui-base
               :qtools-ui-container))
