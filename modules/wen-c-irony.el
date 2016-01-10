;;; 'wen-c-irony.el' --- Emacs c/c++ selection

(wen-require-packages '(
                        irony
                        irony-eldoc
                        company-irony
                        flycheck-irony
                        ))

(require 'irony)
(require 'irony-eldoc)
(require 'company-irony)
(require 'flycheck-irony)


;; irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; eldoc
(add-hook 'irony-mode-hook 'irony-eldoc)

;; irony company
(add-to-list 'company-backends 'company-irony)


;; irony flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(provide 'wen-c-irony)
