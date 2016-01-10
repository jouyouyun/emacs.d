;;; 'wen-c-ycmd.el' --- Emacs c/c++ selection

(wen-require-packages '(
                        ycmd
                        company-ycmd
                        flycheck-ycmd
                        ))

(require 'ycmd)

(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'c++-mode-hook 'ycmd-mode)

;; company-ycmd
;; depends: ycmd
(require 'company-ycmd)
(company-ycmd-setup)
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
;; TODO: compile ycmd
(set-variable 'ycmd-server-command '("python" "/Data/Projects/Private/ycmd/ycmd"))
(set-variable 'ycmd-global-config "~/.ycm_extra_conf.py")

(provide 'wen-c-ycmd)
