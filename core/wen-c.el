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
(defun wen-pkg-config-enable-clang-flag (pkg-config-lib)
  "This function will add necessary header file path of a
specified by `pkg-config-lib' to `company-clang-arguments', which make it
completionable by company-clang"
  (interactive "spkg-config lib: ")
  (if (executable-find "pkg-config")
      (if (= (shell-command
              (format "pkg-config %s" pkg-config-lib))
             0)
          (setq company-clang-arguments
                (append company-clang-arguments
                        (split-string
                         (shell-command-to-string
                          (format "pkg-config --cflags-only-I %s"
                                  pkg-config-lib)))))
        (message "Error, pkg-config lib %s not found." pkg-config-lib))
    (message "Error: pkg-config tool not found.")))

(defun set-common-clang-args ()
  (setq command "echo | g++ -v -x c++ -E - 2>&1 |
                 grep -A 20 starts | grep include | grep -v search")
  (setq company-clang-arguments
        (mapcar (lambda (item)
                  (concat "-I" item))
                (split-string
                 (shell-command-to-string command))))
  )

(defun wen-set-c-clang-args ()
  (set-common-clang-args)
  (wen-pkg-config-enable-clang-flag "gtk+-3.0")
  (wen-pkg-config-enable-clang-flag "fontconfig")
  (wen-pkg-config-enable-clang-flag "popper-glib")
  (wen-pkg-config-enable-clang-flag "libpulse-mainloop-glib")
  (wen-pkg-config-enable-clang-flag "librsvg-2.0")
  (set 'flycheck-clang-args company-clang-arguments)
  )

(defun wen-set-cpp-clang-args ()
  (set-common-clang-args)
  (wen-pkg-config-enable-clang-flag "Qt5Core")
  (wen-pkg-config-enable-clang-flag "Qt5Gui")
  (wen-pkg-config-enable-clang-flag "Qt5Widgets")
  (wen-pkg-config-enable-clang-flag "Qt5DBus")
  (wen-pkg-config-enable-clang-flag "Qt5Network")
  (wen-pkg-config-enable-clang-flag "Qt5Sql")
  (wen-pkg-config-enable-clang-flag "Qt5Svg")
  (wen-pkg-config-enable-clang-flag "Qt5Xml")
  (set 'flycheck-clang-args company-clang-arguments)
  )

(add-hook 'c-mode-hook 'wen-set-c-clang-args)
(add-hook 'c++-mode-hook 'wen-set-cpp-clang-args)

(provide 'wen-c)
