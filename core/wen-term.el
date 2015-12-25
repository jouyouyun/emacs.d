;;; 'wen-term.el' --- Emacs term && shell selection.
;;; Using 'exec-path-from-shell' copy custom environment in shell.
;;; Using 'multi-term' as default term.

(require 'exec-path-from-shell)

(exec-path-from-shell-initialize)
;; Custom env
(dolist (var
	  '("PATH"
	    "HOME"
	    "PRJ"
	    "SYNC"
	    "CONFIG"
            "GOPATH"
            "GOPKG"
	    "GOSRC"))
  (exec-path-from-shell-copy-env var))

(require 'multi-term)
;; multi-term
(global-set-key (kbd "C-c M-t") 'multi-term)
(setq multi-term-program "/bin/zsh"
      ;; TERM is restored to xterm-256-color after that.
      term-term-name "xterm-256color")
;; black
(setq term-default-bg-color "#000000")
;; yellow
(setq term-default-fg-color "#dddd00")

;; update current directory
(defadvice term-send-input (after update-current-directory)
  "Update the current directory."
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd (file-truename (format "/proc/%d/cwd" pid))))
    (cd cwd)))


(defadvice term-send-raw (after update-current-directory)
  "Update the current directory."
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd (file-truename (format "/proc/%d/cwd" pid))))
    (cd cwd)))

(eval-after-load "term"
  `(progn
     (ad-activate 'term-send-raw)
     (ad-activate 'term-send-input)
     ;; no limit buffer length
     (setq show-trailing-whitespace nil)
     ;; send 'ESC' to terminal
     (add-to-list 'term-bind-key-alist '("C-c M-e" . term-send-esc))
     (setq term-bind-key-alist
           (list (cons "C-c C-c" 'term-interrupt-subjob)
                 ;; jump terminals
                 (cons "C-c M-[" 'multi-term-prev)
                 (cons "C-c M-]" 'multi-term-next)
                 (cons "C-p" 'previous-line)
                 (cons "C-n" 'next-line)
                 (cons "M-f" 'term-send-forward-word)
                 (cons "M-b" 'term-send-backward-word)
                 (cons "C-c C-j" 'term-line-mode)
                 (cons "C-c C-k" 'term-char-mode)
                 (cons "M-DEL" 'term-send-backward-kill-word)
                 (cons "M-d" 'term-send-forward-kill-word)
                 (cons "C-r" 'term-send-reverse-search-history)))
     ;; paste
     (define-key term-raw-map (kbd "C-y") 'term-paste)
     ;; Disable yasnippet
     (yas-minor-mode -1)))

(provide 'wen-term)
