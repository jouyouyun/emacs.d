;;; 'wen-misc.el' --- Emacs misc selection

(require 'popup)
(require 'yasnippet)

;; Yasnippet
(eval-after-load 'popup
  '(progn
     (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
     (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
     (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
     (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
     (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t)))

;; Hacking to 'yas-prompt-functions', it adopts 'popup'
(setq yas-prompt-functions
      '(yas-popup-isearch-prompt
        yas-no-prompt))
(yas-global-mode 1)
;; yasnippet conflict with auto complete for 'TAB'
;; so unset 'TAB' in yasnippet
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Open the search url constructed with the QUERY_URL.
;; PROMPT sets the 'read-string' prompt.
(defun wen-search-engine (query-url prompt)
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt)))
           ))
  )

;; Given some information regarding a search engine, install the interactive
;; command to search through them.
(defmacro wen-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  `(defun ,(intern (format "wen-%s" search-engine-name)) ()
          ,(format "Search %s with a query or region if any." search-engine-name)
          (interactive)
          (wen-search-engine ,search-engine-url ,search-engine-prompt)
          )
  )

(wen-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(wen-install-search-engine "baidu"     "http://www.baidu.com/search?q="              "Baidu: ")
(wen-install-search-engine "github"     "https://github.com/search?q="                 "GitHub: ")

(global-set-key (kbd "C-c s g") 'wen-google)
(global-set-key (kbd "C-c s G") 'wen-github)
(global-set-key (kbd "C-c s b") 'wen-baidu)

(provide 'wen-misc)
