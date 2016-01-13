;;; 'wen-c.el' --- Emacs c/c++ selection

(wen-require-packages '(
                        c-eldoc
			company-c-headers
                        ))

(require 'c-eldoc)

;; c-mode
(defun wen-cc-mode-setup()
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "k&r")
                          (c++-mode . "stroustrup")
                          (other . "linux")))
  (setq-default c-basic-offset 4)
  (add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
  (delete 'company-semantic company-backends)
  )

(wen-cc-mode-setup)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)

;; company-c-header
;; TODO: set 'company-c-headers-path-system'
(require 'company-c-headers)
(defun wen-c-headers-company ()
  (add-to-list 'company-backends 'company-c-headers))

(add-hook 'c-mode-hook 'wen-c-headers-company)
(add-hook 'c++-mode-hook 'wen-c-headers-company)

;; autocomplete via company-clang
(defvar wen-c-clang-args '(
                           "-I/usr/include/glib-2.0"
                           "-I/usr/include/gtk-3.0/gtk"
                           "-I/usr/include/gtk-3.0/gdk"
                           "-I/usr/include/cairo"
                           "-I/usr/include/librsvg-2.0"
                           "-I/usr/include/gdk-pixbuf-2.0/gdk-pixbuf"
                           "-I/usr/include/gdk-pixbuf-2.0/gdk-pixbuf-xlib"
                           "-I/usr/include/poppler/glib"
                           "-I/usr/include/fontconfig"
                           "-I/usr/include/freetype2"
                           "-I/usr/include/lightdm-gobject-1"
                           "-I/usr/include/X11"
                           "-I/usr/include/X11/Xcursor"
                           "-I/usr/include/X11/extensions"
                           "-I/usr/include/xcb"
                           "-I/usr/include/pulse"
                           "-D_DEBUG"
                           ))

;; (setq wen-c-clang-args
;;       (substring
;;        (shell-command-to-string "/usr/bin/pkg-config --cflags --libs gtk+-3.0 x11 xtst xcursor")
;;        0 -1))


(defvar wen-cpp-clang-args '(
                             "-I/usr/include/c++/5"
                             ;; deepin
                             ;;"-I/usr/include/x86_64-linux-gnu/qt5/QtGui"
                             ;;"-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets"
                             ;;"-I/usr/include/x86_64-linux-gnu/qt5/QtDBus"
                             ;;"-I/usr/include/x86_64-linux-gnu/qt5/QtSql"
                             ;;"-I/usr/include/x86_64-linux-gnu/qt5/QtSvg"
                             ;;"-I/usr/include/x86_64-linux-gnu/qt5/QtXml"
                             ;;"-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork"
                             ;; arch
                             ;;"-I/usr/include/qt/QtCore"
                             ;;"-I/usr/include/qt/QtGui"
                             ;;"-I/usr/include/qt/QtWidgets"
                             ;;"-I/usr/include/qt/QtDBus"
                             ;;"-I/usr/include/qt/QtSql"
                             ;;"-I/usr/include/qt/QtSvg"
                             ;;"-I/usr/include/qt/QtXml"
                             ;;"-I/usr/include/qt/QtNetwork"
                             ;; "-I/usr/include/qt/QtX11Extras"
                             ;;"-DQT_CORE_LIB"
                             ;;"-DQT_GUI_LIB"
                             ;;"-DQT_NETWORK_LIB"
                             ;;"-DQT_QML_LIB"
                             ;;"-DQT_SQL_LIB"
                             ;;"-DQT_WIDGETS_LIB"
                             ;;"-DQT_XML_LIB"
                             "-D_DEBUG"
                             ))
;; (setq wen-cpp-clang-args
;;       (substring
;;        (shell-command-to-string "/usr/bin/pkg-config --cflags --libs Qt5Core Qt5Gui Qt5DBus Qt5Network Qt5Sql Qt5Svg Qt5Widgets")
;;        0 -1))

(defun wen-set-clang-args (args)
  (set 'company-clang-arguments args)
  (set 'flycheck-clang-args args))

(defun wen-set-c-clang-args ()
  (wen-set-clang-args wen-c-clang-args))

(defun wen-set-cpp-clang-args ()
  (wen-set-clang-args wen-cpp-clang-args))

(add-hook 'c-mode-hook 'wen-set-c-clang-args)
(add-hook 'c++-mode-hook 'wen-set-cpp-clang-args)

(provide 'wen-c)
