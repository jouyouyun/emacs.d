;;; init.el --- Wen's configuration entry point.

(when (version< emacs-version "24.1")
  (error "Wen requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

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
;; This files contains a list of modules that will be loaded.
(defvar wen-modules-file (expand-file-name "wen-modules.el" wen-dir))
;;; Define load dir end ...

(unless (file-exists-p wen-savefile-dir)
  (make-directory wen-savefile-dir))

(defun wen-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
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
(when (file-exists-p wen-personal-preload-dir)
  (message "Loading personal configuration files in %s..." wen-personal-preload-dir)
  (mapc 'load (directory-files wen-personal-preload-dir 't "^[^#].*el$")))

(message "Loading Wen's core...")

;; the core stuff
(require 'wen-packages)
(require 'wen-custom)  ;; Needs to be loaded before core, editor and ui
(require 'wen-shell-env)
(require 'wen-ui)
(require 'wen-core)
(require 'wen-mode)
(require 'wen-editor)
(require 'wen-global-keybindings)
(require 'wen-pyim)
(require 'wen-window)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'wen-osx))

(message "Loading Wen's modules...")

;; the modules
(if (file-exists-p wen-modules-file)
    (load wen-modules-file)
  (message "Missing modules file %s" wen-modules-file)
  (message "You can get started by copying the bundled example file"))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" wen-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p wen-personal-dir)
  (message "Loading personal configuration files in %s..." wen-personal-dir)
  (mapc 'load (directory-files wen-personal-dir 't "^[^#].*el$")))


(wen-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'wen-tip-of-the-day))

;;; init.el ends here
