;;; init.el --- Wen's configuration entry point.

(when (version< emacs-version "24.1")
  (error "Wen requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; enable debug
(setq debug-on-error t)

;; FIXME: Reset `tramp-ssh-controlmaster-options' to fix startup slow
;; issue since 24.5.
(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; Always load newest byte code
(setq load-prefer-newer t)

;;; Define load dir
;;;
;; The root dir of the Emacs distribution.
(defvar wen-dir (file-name-directory load-file-name))
;; The home of Wen's core functionality.
(defvar wen-core-dir (expand-file-name "core" wen-dir))
;; This directory houses all of the built-in modules.
(defvar wen-modules-dir (expand-file-name  "modules" wen-dir))
;; This directory is for your personal configuration.
;; Users of Emacs are encouraged to keep their personal configuration
;; changes in this directory.  All Emacs Lisp files there are loaded automatically.
(defvar wen-personal-dir (expand-file-name "personal" wen-dir))
;; This directory houses packages that are not yet available in ELPA (or MELPA).
(defvar wen-personal-preload-dir (expand-file-name "preload" wen-personal-dir))
;; This folder stores all the automatically generated save/history-files.
(defvar wen-savefile-dir (expand-file-name "savefile" wen-dir))

(unless (file-exists-p wen-savefile-dir)
  (make-directory wen-savefile-dir))

;; Add all level PARENT-DIR subdirs to the `load-path'.
(defun wen-add-subfolders-to-load-path (parent-dir)
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (wen-add-subfolders-to-load-path name)))))

;; add Wen's directories to Emacs's `load-path'
(add-to-list 'load-path wen-core-dir)
(add-to-list 'load-path wen-modules-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `wen-personal-preload-dir'
;; (when (file-exists-p wen-personal-preload-dir)
;;   (message "Loading personal configuration files in %s..." wen-personal-preload-dir)
;;   (mapc 'load (directory-files wen-personal-preload-dir 't "^[^#].*el$")))

(message "Loading Wen's core...")

;; the core stuff
(require 'wen-packages)
(require 'wen-ui)
(require 'wen-basic)
(require 'wen-misc)
(require 'wen-term) ;; must load after 'wen-misc'
(require 'wen-company)
(require 'wen-helm)
(require 'wen-editor)
(require 'wen-window)
(require 'wen-c)
(require 'wen-orgmode)
(require 'wen-git)
(require 'wen-files)
(require 'wen-pinyin)
(require 'wen-tip)

(message "Loading Wen's modules...")

;; the modules
(require 'wen-go)
(require 'wen-python)
(require 'wen-qml)
(require 'wen-web)

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" wen-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p wen-personal-dir)
  (message "Loading personal configuration files in %s..." wen-personal-dir)
  (mapc 'load (directory-files wen-personal-dir 't "^[^#].*el$")))


(wen-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'wen-tip-of-the-day))

;; server mode
(server-start)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; set variables
(custom-set-variables
 '(auto-save-default nil)
 '(make-backup-files nil)
 '(x-select-enable-clipboard t)
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(display-time-24hr-format t)
 ;; '(display-time-day-and-date t)
 ;; '(display-time-format "%m月%d日%A%H:%M")
 ;; '(display-time-interval 10)
 ;; '(display-time-mode 1)
 )

;;; init.el ends here
