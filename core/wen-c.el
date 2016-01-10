;;; 'wen-c.el' --- Emacs c/c++ selection

(wen-require-packages '(
                        c-eldoc
			cpputils-cmake
			company-c-headers
                        ))

(require 'c-eldoc)

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
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)


;; cpputils-cmake
(require 'cpputils-cmake)

(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))

;; company-c-header
;; TODO: set 'company-c-headers-path-system'
(require 'company-c-headers)
(defun wen-c-headers-company ()
  (add-to-list 'company-backends 'company-c-headers))

(add-hook 'c-mode-hook 'wen-c-headers-company)
(add-hook 'c++-mode-hook 'wen-c-headers-company)

(provide 'wen-c)
