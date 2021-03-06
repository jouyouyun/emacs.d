;;; 'wen-web.el' --- Emacs web(html/css/js) selection.

(wen-require-packages '(
                        js2-mode
                        company-web
                        web-completion-data
                        company-tern
                        coffee-mode
                        ))

;; (require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
'("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;; company-web
;; depends: dash, web-completion-data
(require 'company-web-html)
(require 'company-tern)
(defun wen-web-mode-hook ()
  "Hook for `web-mode'."
  (set (make-local-variable 'company-backends)
       '(
         company-dabbrev
         company-keywords
         company-tern
         company-web-html
         company-yasnippet
         company-files)))

;; Enable JavaScript completion between <script>...</script> etc.
(defadvice company-tern (before web-mode-set-up-ac-sources activate)
  "Set `tern-mode' based on current language before running company-tern."
  (message "advice")
  (if (equal major-mode 'web-mode)
      (let ((web-mode-cur-language
             (web-mode-language-at-pos)))
        (if (or (string= web-mode-cur-language "javascript")
                (string= web-mode-cur-language "jsx")
                )
            (unless tern-mode (tern-mode))
          (if tern-mode (tern-mode -1))))))

(eval-after-load 'web-mode
  `(progn
     (add-hook 'web-mode-hook 'wen-web-mode-hook)
     ;; manual autocomplete
     (define-key web-mode-map (kbd "M-SPC") 'company-complete)
     ))

;; coffee
(eval-after-load 'coffee-mode
  '(progn
     ;; automatically clean up bad whitespace
     (setq whitespace-action '(auto-cleanup))
     ;; only show bad whitespace
     (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
     ;; CoffeeScript uses two spaces.
     (setq coffee-tab-width 8)

     ;; remove the "Generated by CoffeeScript" header
     (add-to-list 'coffee-args-compile "--no-header")

     (defun wen-coffee-mode-defaults ()
       ;; Update the already compiled js on save
       (and (buffer-file-name)
            (file-exists-p (buffer-file-name))
            (file-exists-p (coffee-compiled-file-name (buffer-file-name)))
            (coffee-cos-mode t))
       (subword-mode +1))

     (setq wen-coffee-mode-hook 'wen-coffee-mode-defaults)

     (add-hook 'coffee-mode-hook (lambda ()
                                   (run-hooks 'wen-coffee-mode-hook)))))

;; js
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (defun wen-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq-local electric-layout-rules '((?\; . after)))
       (setq mode-name "JS2")
       (js2-imenu-extras-mode +1))

     (setq wen-js-mode-hook 'wen-js-mode-defaults)

     (add-hook 'js2-mode-hook (lambda () (run-hooks 'wen-js-mode-hook)))))

;; css
(eval-after-load 'css-mode
  '(progn
     (wen-require-packages '(rainbow-mode))

     (setq css-indent-offset 2)

     (defun wen-css-mode-defaults ()
       (rainbow-mode +1)
       (run-hooks 'wen-prog-mode-hook))

     (setq wen-css-mode-hook 'wen-css-mode-defaults)

     (add-hook 'css-mode-hook (lambda ()
                                (run-hooks 'wen-css-mode-hook)))))

(provide 'wen-web)
