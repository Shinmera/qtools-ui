#|
 This file is a part of Qtools-UI
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#

(asdf:defsystem qtools-ui-auto-resizing-textedit
  :license "zlib"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :description "QTextEdit with automatic height adjustment"
  :homepage "https://Shinmera.github.io/qtools-ui/"
  :bug-tracker "https://github.com/Shinmera/qtools-ui/issues"
  :source-control (:git "https://github.com/Shinmera/qtools-ui.git")
  :serial T
  :components ((:file "auto-resizing-textedit"))
  :depends-on (:qtools-ui-base
               :qtools-ui-fixed-qtextedit))
