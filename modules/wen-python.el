;;; 'wen-python.el' --- Emacs python selection.

(wen-require-packages '(
                        python-mode
                        anaconda-mode
                        pythonic
                        company-anaconda
                        ))

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; company-anaconda: complete for python
;; depends: anaconda, pythonic
(require 'pythonic)
(require 'anaconda-mode)
(require 'company-anaconda)

(eval-after-load "python"
  `(progn
     (add-hook 'python-mode-hook 'anaconda-mode)
     (add-hook 'python-mode-hook 'eldoc-mode)
     (add-to-list 'company-backends 'company-anaconda)
     ))

(provide 'wen-python)
