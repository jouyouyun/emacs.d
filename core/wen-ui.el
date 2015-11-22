;;; 'wen-ui' --- Emacs ui selection.

(require 'linum)

;; disable startup screen
(setq inhibit-startup-screen t)

;; use zenburn as the default theme
(load-theme 'zenburn t)

;; mode line settings
(global-linum-mode t)
(column-number-mode t)
(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Wen - " (:eval (if (buffer-file-name)
                                                (abbreviate-file-name (buffer-file-name))
                                              "%b"))))

;; show the cursor when moving after big movements in the window
(require 'beacon)
(beacon-mode +1)

;; Disable menubar, toolbar and scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Maximized
(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; Setting English Font
;; (if (member "Source Code Pro" (font-family-list))
;;     (set-face-attribute
;;      'default nil :font "Source Code Pro 10"))
;; Font size
;; The value is in 1/10pt, so 100 will give you 10pt
(set-face-attribute 'default nil :height 100)
;; Set transparent effect
(setq alpha-list '((90 60) (100 100)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list))) ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
  )
;; Set alpha (90, 60)
;; (loop-alpha)

(provide 'wen-ui)
