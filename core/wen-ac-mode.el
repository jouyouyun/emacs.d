;;; 'wen-ac-mode.el' -- Auto complete config.

(wen-require-packages '(
			auto-complete
                        ac-c-headers
			))

(require 'auto-complete-config)

(ac-config-default)
(setq ac-auto-start 3)
(setq ac-use-fuzzy t)
(define-key ac-mode-map (kbd "C-x C-o") 'auto-fuzzy-complete)
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Ignore case if completion target string doesn't include upper characters
(setq ac-ignore-case 'smart)

;; Examples
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

;; Source
(setq-default ac-sources '(ac-source-words-in-all-buffer))

(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(add-to-list 'ac-modes 'c-mode)
(add-to-list 'ac-modes 'c++-mode)

(add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))


(provide 'wen-ac-mode)
