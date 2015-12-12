;;; wen-packages.el --- Emacs default package selection.
;;; Code copy from Prelude.

(require 'cl)
(require 'package)

;; melpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; orgmode
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

;; set package-user-dir to be relative to install path
(setq package-user-dir (expand-file-name "elpa" wen-dir))
(package-initialize)

;; a list of packages to ensure are installed at launch.
(defvar wen-packages
  '(helm
    helm-firefox
    helm-ls-git
    avy
    multiple-cursors
    ;; browse-kill-ring ;; see 'popup-kill-ring'
    easy-kill
    whole-line-or-region
    expand-region
    autopair
    rainbow-delimiters
    anzu
    indent-guide
    ace-window
    gist
    magit
    git-gutter-fringe
    git-timemachine
    gitconfig-mode
    gitignore-mode
    vlf
    json-mode
    json-reformat
    markdown-mode
    syslog-mode
    exec-path-from-shell
    multi-term
    dash
    undo-tree
    diff-hl
    beacon
    flycheck
    projectile
    company
    popup
    pos-tip
    popup-kill-ring
    yasnippet
    chinese-pyim
    youdao-dictionary
    zenburn-theme))

;; Check if all packages in 'wen-packages' are installed
(defun wen-packages-installed-p ()
  (every #'package-installed-p wen-packages))

;; Ensure PACKAGES are installed.
;; Missing packages are installed automatically.
(defun wen-require-packages (packages)
  (mapc #'wen-require-package packages))

;; Install PACKAGE unless already installed.
(defun wen-require-package (package)
  (unless (memq package wen-packages)
    (add-to-list 'wen-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(define-obsolete-function-alias 'wen-ensure-module-deps 'wen-require-packages)

;; Install all packages listed in 'wen-packages'.
(defun wen-install-packages ()
  ;; check for new packages (package versions)
  (unless (wen-packages-installed-p)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (wen-require-packages wen-packages)))

;; run package installation
(wen-install-packages)

;; Browse third-party packages not bundled with Wen.
;; Behaves similarly to `package-list-packages',
;; but shows only the packages that
;; are installed and are not in `prelude-packages'.
;; Useful for removing unwanted packages.
(defun wen-list-foreign-packages ()
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list wen-packages)))


;; When file with EXTENSION is opened triggers auto-install of PACKAGE.
;; PACKAGE is installed only if not already present.
;; The file is opened in MODE.
(defmacro wen-auto-install (extension package mode)
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar wen-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("\\.log\\'" syslog-mode syslog-mode)
    ("\\.error\\'" syslog-mode syslog-mode)
    ("\\syslog\\'" syslog-mode syslog-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (wen-auto-install extension package mode))))
 wen-auto-install-alist)

(require 'epl)

(defun wen-update ()
  "Update Wen to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update Wen? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Updating Wen...")
    (cd wen-dir)
    (shell-command "git pull")
    (wen-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun wen-update-packages (&optional arg)
  "Update Wen's packages.
This includes package installed via `wen-require-package'.

With a prefix ARG updates all installed packages."
  (interactive "P")
  (when (y-or-n-p "Do you want to update Wen's packages? ")
    (if arg
        (epl-upgrade)
      (epl-upgrade (-filter (lambda (p) (memq (epl-package-name p) wen-packages))
                            (epl-installed-packages))))
    (message "Update finished. Restart Emacs to complete the process.")))

(defun wen-recompile-init ()
   "Byte-compile all your dotfiles again."
   (interactive)
   (byte-recompile-directory wen-dir 0))

(provide 'wen-packages)
