;;; 'wen-go.el' --- Emacs golang selection.

(wen-require-packages '(
                        go-mode
                        flymake-go
                        go-eldoc
                        gotest
                        company-go
                        ))

(require 'go-mode)
(require 'flymake-go)
(require 'go-eldoc)
(require 'gotest)

(eval-after-load "go-mode"
  `(progn
     (add-hook 'go-mode-hook '(lambda ()
                                (yas-minor-mode t)
                                (setq-default tab-width 8)
                                (local-set-key (kbd "C-c C-f") 'gofmt)
                                (local-set-key (kbd "C-c C-k") 'godoc)
                                ;; go-import-add is bound to "C-c C-a" by default
                                (local-set-key (kbd "C-c C-r")
                                               'go-remove-unused-imports)
                                (local-set-key (kbd "C-c C-g") 'go-goto-imports)
                                (local-set-key (kbd "C-c C-")
                                               'go-direx-pop-to-buffer)
                                ;; gofmt on save
                                (add-hook 'before-save-hook
                                          'gofmt-before-save nil t)
                                (go-eldoc-setup t)
                                ;; gotest
                                (let ((map go-mode-map))
                                  ;; current package, really
                                  (define-key map (kbd "C-c a")
                                    'go-test-current-project)
                                  (define-key map (kbd "C-c m")
                                    'go-test-current-file)
                                  (define-key map (kbd "C-c .")
                                    'go-test-current-test)
                                  (define-key map (kbd "C-c b") 'go-run)
                                  (define-key map (kbd "C-h f") 'godoc-at-point))
                                ))))

;; company-go
;; Must install gcode before used
;; depends: go-mode
(require 'company-go)
(eval-after-load "go-mode"
  `(progn
     (add-hook 'go-mode-hook '(lambda ()
                               (add-to-list 'company-backends 'company-go)
                               ))))

(provide 'wen-go)
