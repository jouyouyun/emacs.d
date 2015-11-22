;;; 'wen-qml.el' --- Emacs qml selection.

(wen-require-packages '(
                        qml-mode
                        company-qml
                        ))

(require 'qml-mode)

;; company-qml
;; depends: qml-mode
;; Use 'locate plugins.' get list, use 'sudo updatedb' to gen locate datebase
(require 'company-qml)

(eval-after-load "qml-mode"
  `(progn
     (add-to-list 'company-backends 'company-qml)
     (setq qmltypes-parser-file-list '(
                                       "/usr/lib/qt/qml/QtQuick/Controls/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick/Dialogs/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick/Extras/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick/Layouts/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick/LocalStorage/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick/Particles.2/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick/PrivateWidgets/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick/Window.2/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick/XmlListModel/plugins.qmltypes"
                                       "/usr/lib/qt/qml/QtQuick.2/plugins.qmltypes"))
     ))

(provide 'wen-qml)
