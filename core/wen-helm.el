;;; 'wen-helm.el' --- Emacs helm selection

(require 'helm-config)
(require 'helm-ls-git)

(helm-mode 1)
(setq helm-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -name %s %s")
        (t "locate %s")))

(global-set-key (kbd "C-x c g") 'helm-do-grep)
(global-set-key (kbd "C-x c o") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions using C-z
;; (define-key helm-map (kbd "C-z") 'helm-select-action)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq
 ;; open helm buffer inside current window, not occupy whole other window
 helm-split-window-in-side-p t
 ;; fuzzy matching buffer names when non--nil
 helm-buffers-fuzzy-matching t
 ;; move to end or beginning of source when reaching top or bottom of source.
 helm-move-to-line-cycle-in-source t
 ;; search for library in `require' and `declare-function' sexp.
 helm-ff-search-library-in-sexp t
 ;; scroll 8 lines other window using M-<next>/M-<prior>
 helm-scroll-amount 8
 helm-ff-file-name-history-use-recentf t)

;; Helm-ag
(when (executable-find "ag")
  (wen-require-package 'helm-ag)
  (global-set-key (kbd "M-?") 'helm-ag)
  ;; (custom-set-variables
  ;;  '(helm-follow-mode-persistent t))
  )


(provide 'wen-helm)
