;;; 'wen-basic.el' --- Emacs basic selection.

;; disable autosave
(setq auto-save-default nil)
;; disable backup file
(setq make-backup-files nil)
;; enable clipboard
(setq x-select-enable-clipboard t)
;; set environment coding system
(set-language-environment "UTF-8")
;; auto revert buffer globally
(global-auto-revert-mode t)
;; set TAB and indention
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; y or n is suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)
;; always add new line to the end of a file
(setq require-final-newline nil)
;; add no new lines when "arrow-down key" at the end of a buffer
(setq next-line-add-newlines nil)
;; prevent the annoying beep on errors
(setq ring-bell-function 'ignore)
;; remove trailing whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; enable to support navigate in camelCase words
(global-subword-mode t)
;; hide startup splash screen
(setq inhibit-startup-screen t)
;; set text-mode as the default major mode, instead of fundamental-mode
;; The first of the two lines in parentheses tells Emacs to turn on Text mode
;; when you find a file, unless that file should go into some other mode, such
;; as C mode.
(setq-default major-mode 'text-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; Recentf
(require 'recentf)
;; save the .recentf file to .emacs.d/
(setq recentf-save-file (concat wen-savefile-dir "/recentf"))
;; enable recent files mode.
(recentf-mode t)
;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;; ido-mode
(setq ido-enable-prefix nil)
(setq ido-enable-case nil)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Save Place in Opened Files
;;
;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file. The following code comes from
;; http://emacs-fu.blogspot.com/2009/05/remembering-your-position-in-file.html.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat wen-savefile-dir "/saved-places"))

;; flycheck
(require 'flycheck)
(eval-after-load 'flycheck
  '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
(add-hook 'prog-mode-hook 'flycheck-mode)

;; indent-guide
;;(require 'indent-guide)

;; Locales
;; Return whether locale string V relates to a UTF-8 locale.
(defun utf8-locale-p (v)
  (and v (string-match "UTF-8" v)))

;; Return t if the \"locale\" command or environment variables prefer UTF-8.
(defun locale-is-utf8-p ()
  (or (utf8-locale-p (and (executable-find "locale")
                          (shell-command-to-string "locale")))
      (utf8-locale-p (getenv "LC_ALL"))
      (utf8-locale-p (getenv "LC_CTYPE"))
      (utf8-locale-p (getenv "LANG"))))

(when (or window-system (locale-is-utf8-p))
  ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  (setq utf-translate-cjk-mode nil)
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt)
                                   'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))


;; fiplr
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-x f") 'fiplr-find-file)


(provide 'wen-basic)
