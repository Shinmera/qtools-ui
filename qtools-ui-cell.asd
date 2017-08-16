#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-cell
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An item-widget that presents a draggable and selectable cell."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "cell"))
  :depends-on (:qtools-ui-base
               :qtools-ui-layout
               :qtools-ui-helpers))
