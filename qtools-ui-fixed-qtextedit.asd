#|
 This file is a part of Qtools-UI
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#

(asdf:defsystem qtools-ui-fixed-qtextedit
  :license "Artistic"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :description "QTextEdit with working context menu - workaround for QTBUG-9592"
  :homepage "https://Shinmera.github.io/qtools-ui/"
  :bug-tracker "https://github.com/Shinmera/qtools-ui/issues"
  :source-control (:git "https://github.com/Shinmera/qtools-ui.git")
  :serial T
  :components ((:file "fixed-qtextedit"))
  :depends-on (:qtools-ui-base))
