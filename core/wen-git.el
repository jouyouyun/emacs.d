;;; 'wen-git.el' -- Emacs git selection.

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'git-rebase-mode-hook
          (lambda ()
            (evil-local-mode -1)))

(require 'git-gutter-fringe)
(global-git-gutter-mode)

(dolist (mode-hook '(text-mode-hook prog-mode-hook))
  (add-hook mode-hook
            (lambda ()
              ;; set fringe width to better display
              (setq left-fringe-width 10)
              (setq right-fringe-width 4))))

;; some keybindings
(global-set-key (kbd "C-x v g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Jump to next/previous hunk
(global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;; improve performance
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

;; gist
;;    g : reload the gist list from server
;;    e : edit current gist description
;;    k : delete current gist
;;    + : add a file to the current gist
;;    - : remove a file from the current gist
;;    y : print current gist url
;;    b : browse current gist
;;    * : star gist
;;    ^ : unstar gist
;;    f : fork gist

;; git-timemachine
;;    p Visit previous historic version
;;    n Visit next historic version
;;    w Copy the abbreviated hash of the current historic version
;;    W Copy the full hash of the current historic version
;;    g Goto nth revision
;;    q Exit the time machine.

(provide 'wen-git)
