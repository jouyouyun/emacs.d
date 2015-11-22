;;; 'wen-misc.el' --- Emacs misc selection

(require 'popup)
(require 'yasnippet)

;; Yasnippet
(eval-after-load 'popup
  '(progn
     (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
     (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
     (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
     (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
     (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t)))

;; Hacking to 'yas-prompt-functions', it adopts 'popup'
(setq yas-prompt-functions
      '(yas-popup-isearch-prompt
        yas-no-prompt))
(yas-global-mode 1)
;; yasnippet conflict with auto complete for 'TAB'
;; so unset 'TAB' in yasnippet
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
 
(provide 'wen-misc)
