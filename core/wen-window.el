;;; wen-window.el --- Emacs window and buffer selection.
;;;
;;; Using 'ace-window' to jump in windows.
;;; Depends: avy, ring

(require 'ace-window)

;; Default <C-u>
;; <M-p> calling 'ace-window', double to cancel 'ace-window'
(global-set-key (kbd "M-p") 'ace-window)
;; aw-keys default 0-9
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; aw-scope default 'global' show all windows, 
;; if set to 'frame' show windows only in current frame.
(setq aw-background nil)
(setq aw-dispatch-always t)
;; List of actions for `aw-dispatch-default'.
(defvar aw-dispatch-alist
'((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window " Ace - Select the previous window")
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows " Ace - Maximize current window"))
)

(provide 'wen-window)
