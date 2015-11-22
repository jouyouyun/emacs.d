;;; 'wen-company.el' --- Emacs company complete selection.

(wen-require-packages '(
                        company-flx
                        company-try-hard
                        company-quickhelp
                        helm-company
                        readline-complete
                        company-math
                        ))
(require 'company)

;; Use 'company-mode' in all buffers.
;; Completion will start automatically after you type a few letters.
;; Use M-n and M-p to select, <return> to complete or
;; <tab> to complete the common part. Search through the completions
;; with C-s, C-r and C-o. Press M-(digit) to quickly complete with
;; one of the first 10 candidates.
(setq company-idle-delay 0.3)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
;; start autocompletion only after typing
(setq company-begin-commands '(self-insert-command))
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
(add-hook 'after-init-hook 'global-company-mode)
;; company-dabbrev
;; (setq company-dabbrev-code-everywhere t)
(global-set-key (kbd "M-/") 'company-dabbrev)
;; Customsize company backends
(setq-local company-backends '((
                                company-capf
                                company-dabbrev
                                company-files
                                company-abbrev
                                company-keywords
                                company-semantic
                                company-yasnippet
                                company-clang
                                company-css
                                company-cmake
                                company-elisp
                                company-etags
                                company-nxml
                                company-pkg
                                )))
;; color
(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))

;; company-fix
(require 'company-flx)
(eval-after-load "company"
  (company-flx-mode +1))

;; company-try-hard
;; get all completions from company backends
(require 'company-try-hard)
(global-set-key (kbd "C-x C-z") #'company-try-hard)
(define-key company-active-map (kbd "C-z") #'company-try-hard)

;; company-quickhelp
(company-quickhelp-mode 1)

;; helm-company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; readline-complete
;; complete for emacs shell
(require 'readline-complete)
(add-to-list 'company-backends 'readline-complete)

;; company-math for unicode and latex
(require 'company-math)
;; global activation of the unicode symbol completion
(add-to-list 'company-backends 'company-math-symbols-unicode)
;; local configuration for TeX modes
(defun wen-latex-mode-company ()
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-latex-commands)
  )

(eval-after-load "tex-mode"
  `(progn
     (add-hook 'tex-mode-hook 'wen-latex-mode-company)
     ))

(provide 'wen-company)
