#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-ui-layout
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "The basic layout and item-layout components required to build layouts."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "layout")
               (:file "items"))
  :depends-on (:qtools-ui-base))
