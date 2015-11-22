;;; 'wen-c.el' --- Emacs c/c++ selection

(wen-require-packages '(
                        c-eldoc
                        ycmd
                        company-ycmd
                        flycheck-ycmd
                        company-c-headers
                        ))

(require 'c-eldoc)
(require 'ycmd)

;; c-mode
(defun wen-cc-mode-setup()
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "k&r")
                          (c++-mode . "stroustrup")
                          (other . "linux")))
  (setq-default c-basic-offset 4)
  (add-to-list 'auto-mode-alist '("\\.h$" . c-mode)))

(wen-cc-mode-setup)

(add-hook 'c-mode-hook '(lambda ()
                          (ycmd-mode)
                          (c-turn-on-eldoc-mode)
                          ))

(add-hook 'c++-mode-hook '(lambda ()
                            (ycmd-mode)
                            (c-turn-on-eldoc-mode)
                            ))

;; company-ycmd
;; depends: ycmd
(require 'company-ycmd)
(company-ycmd-setup)
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
;; TODO: compile ycmd
(set-variable 'ycmd-server-command '("python" "/Data/Projects/Private/ycmd"))
(set-variable 'ycmd-global-config "~/.ycm_extra_conf.py")

;; company-c-header
;; TODO: set 'company-c-headers-path-system'
(require 'company-c-headers)
(defun wen-c-headers-company ()
  (add-to-list 'company-backends 'company-c-headers))

(add-hook 'c-mode-hook 'wen-c-headers-company)
(add-hook 'c++-mode-hook 'wen-c-headers-company)

(provide 'wen-c)
