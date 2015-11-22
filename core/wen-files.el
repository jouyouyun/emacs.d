;;; 'wen-files.el' --- Emacs files mode selection.
;;; Files like: json, xml, markdown, ini, log, PKGBUILD

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

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
