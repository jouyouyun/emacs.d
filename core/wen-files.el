;;; 'wen-files.el' --- Emacs files mode selection.
;;; Files like: json, xml, markdown, ini, log, PKGBUILD

;; View Large Files
(require 'vlf-setup)

;; shell
(add-to-list 'auto-mode-alist '("\\.sh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.vim$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("vimrc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("zshrc$" . shell-script-mode))

;; conf
(add-to-list 'auto-mode-alist '("\\.gitconfig$" .conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitreview$" .conf-mode))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq markdown-command "pandoc -f markdown -t html")
(setq markdown-css-paths `(,(expand-file-name "github.css" wen-dir)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

(require 'syslog-mode)
(add-to-list 'auto-mode-alist '("\\.log\\'" . syslog-mode))
(add-to-list 'auto-mode-alist '("\\.err\\'" . syslog-mode))
(add-to-list 'auto-mode-alist '("\\.error\\'" . syslog-mode))
(add-to-list 'auto-mode-alist '("\\syslog\\'" . syslog-mode))
(add-to-list 'auto-mode-alist '("\\error\\'" . syslog-mode))

(require 'nxml-mode)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)
;; pom files should be treated as xml files
(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))

(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
(setq nxml-auto-insert-xml-declaration-flag nil)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)

(provide 'wen-files)
