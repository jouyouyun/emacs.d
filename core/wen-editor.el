;;; 'wen-editor.el' --- Emacs editor selection.
;;; Using 'avy' to jump in text.
;;; Using 'multiple-cursors' select many words.

;; avy
(require 'avy)
(avy-setup-default)
(setq avy-background t)
(setq avy-style 'at-full)
(setq avy-style 'at-full)
(global-set-key (kbd "M-g w") 'avy-goto-char-in-line)
(global-set-key (kbd "M-g f") 'avy-goto-char)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g C-y") 'avy-copy-line)
(global-set-key (kbd "M-g C-k") 'avy-move-line)
(global-set-key (kbd "M-g M-y") 'avy-copy-region)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-s") 'mc/skip-to-next-like-this)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; popup-kill-ring
(require 'popup-kill-ring)
(global-set-key (kbd "M-y") 'popup-kill-ring)

;; easy-kill
(require 'easy-kill)
;; For example, M-w w saves current word, repeat w to expand the kill to
;; include the next word. 5 to include the next 5 words etc.
;; The other commands also follow this pattern.
;;
;; +/- does expanding/shrinking according to the thing selected.
;; So for word the expansion is word-wise, for line line-wise,
;; for list or sexp, list-wise.
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "M-s") 'easy-kill)
;; default keybinding
;;  M-w w: save word at point
;;  M-w s: save sexp at point
;;  M-w l: save list at point (enclosing sexp)
;;  M-w d: save defun at point
;;  M-w D: save current defun name
;;  M-w f: save file at point
;;  M-w b: save buffer-file-name or default-directory.
;;         - changes the kill to the directory name,
;;         + to full name and 0 to basename.
;; The following keys modify the selection:
;;  @: append selection to previous kill and exit.
;;     For example, M-w d @ will append current function to last kill.
;;  C-w: kill selection and exit
;;  +, - and 1..9: expand/shrink selection
;;  0: shrink the selection to the intitial size i.e. before any expansion
;;  C-SPC: turn selection into an active region
;;  C-g: abort
;;  ?: help
;;
;; easy mark
(global-set-key [remap mark-sexp] 'easy-mark)

;; Insert an empty line after the current line.
(defun wen-open-line(arg)
  (interactive "P")
  (if arg
      (wen-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

;; Insert an empty line above the current line.
(defun wen-open-line-above()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; A simple wrapper around command `kill-whole-line' that
;; respects indentation.
;; Passes ARG to command `kill-whole-line' when provided.
(defun wen-kill-whole-line (&optional arg)
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key [remap kill-whole-line]
                'wen-kill-whole-line)

;; Move point back to indentation of beginning of line.
;;
;; Move point to the first non-whitespace character on this line.
;; If point is already there, move to the beginning of the line.
;; Effectively toggle between the first non-whitespace character and
;; the beginning of the line.
;;
;; If ARG is not nil or 1, move forward ARG - 1 lines first.  If
;; point reaches the beginning or end of the buffer, stop there."
(defun wen-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'wen-move-beginning-of-line)


(defun wen-delete-whole-word()
  ;; Move to begin of word, delete it
  (interactive)
  (backward-word 1)
  (kill-word 1))

(require 'whole-line-or-region)
;; Comment or uncomment
(global-set-key (kbd "M-;") 'whole-line-or-region-comment-dwim-2)
(global-set-key (kbd "M-w") 'whole-line-or-region-copy-region-as-kill)
(global-set-key (kbd "C-c o") 'wen-open-line)
(global-set-key (kbd "C-c O") 'wen-open-line-above)
(global-set-key (kbd "C-c dd") 'wen-kill-whole-line)
(global-set-key (kbd "C-c x") 'wen-delete-whole-word)

;; 当光标放在行的始端，或者行的中间位置，即为注释该行代码
;; 当光标放在行的末端，即为给该行代码添加注释
;; (defun improve-comment-dwim-line (&optional arg)
;;   "Replacement for the comment-dwim command."
;;   (interactive "*P")
;;   (comment-normalize-vars)
;;   (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
;;       (comment-or-uncomment-region (line-beginning-position) (line-end-position))
;;     (comment-dwim arg)))

;; (global-set-key (kbd "M-;") 'improve-comment-dwim-line)

(require 'autopair)
(autopair-global-mode t)

;; show parenthesis match
;; (show-paren-mode 1)
;; (setq show-paren-style 'expression)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; anzu
(require 'anzu)
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(provide 'wen-editor)
