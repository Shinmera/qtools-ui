#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-drag-and-drop
  :license "Artistic"
  :author "Michał \"phoe\" Herda <phoe@teknik.io>"
  :maintainer "Michał \"phoe\" Herda <phoe@teknik.io>"
  :description "Classes implementing simple drag-and-drop in Qtools."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "drag-and-drop"))
  :depends-on (:qtools-ui-base))
