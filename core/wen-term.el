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
     ;; paste
     (define-key term-raw-map (kbd "C-y") 'term-paste)
     ;; send 'ESC' to terminal
     (add-to-list 'term-bind-key-alist '("C-c M-e" . term-send-esc))
     ;; jump terminals
     (add-to-list 'term-bind-key-alist '("C-c M-[" . multi-term-prev))
     (add-to-list 'term-bind-key-alist '("C-c M-]" . multi-term-next))
     ;; Disable yasnippet
     (yas-minor-mode -1)))

(provide 'wen-term)
