;;; 'wen-ruby.el' --- Emacs ruby selection.

(wen-require-packages '(
                        enh-ruby-mode
                        inf-ruby
						robe
                        ))

;; (require 'enh-ruby-mode)
;; (require 'inf-ruby)
;; (require 'robe)

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
;; (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;; To use enh-ruby-mode for all common Ruby files and the following to your init file
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; If your theme is already defining those faces, just remove the hook with
;; (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

;; Depends: ruby-pry, ruby-method_source
;; Usage: M-x inf-ruby M-x robe-start
(autoload 'robe-mode "robe" "Run an inferior Ruby process" t)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-to-list 'company-backends 'company-robe)

(provide 'wen-ruby)
