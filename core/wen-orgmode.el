;;; 'wen-orgmode.el' --- Emacs Org selection.

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)

(defvar org-directory "/Data/org")

(setq org-default-notes-file (concat org-directory "/captures.org"))

(setq org-agenda-files `(,(concat org-directory "/todos.org")
                         ,(concat org-directory "/todos/work")
                         ,(concat org-directory "/notes/work")))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;; 默认不折叠
(setq org-startup-folded 'showeverything)

;; set supported language
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (C . t)
   (sqlite . t)
   (perl . t)
   (ruby . t)
   (dot . t)
   ))

;; 插入代码
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "c" "go" "sh" "java" "js" "clojure" "c++"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "css")))
     (list (completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;; 导出语法高亮的Source Code
(setq org-src-fontify-natively t)


;; Github Flavored Markdown exporter for Org Mode
(wen-require-package 'ox-gfm)
(require 'ox-gfm)


;; Export Org to html5 slide
(wen-require-package 'ox-html5slide)
(require 'ox-html5slide)

(setq org-capture-templates
      '(("t" "Task" entry (file+headline
                           (concat org-directory "/todos.org")
                           "Tasks")
         "** TODO %?\n\n")
        ("n" "Note" entry (file (concat org-directory "/notes.org"))
         "* %<%D> %^{Title} :NOTE:%^G\n%?")))

(setq org-latex-pdf-process
  '("xelatex -shell-escape -interaction nonstopmode %f"
    "xelatex -shell-escape -interaction nonstopmode %f"
    "xelatex -shell-escape -interaction nonstopmode %f"
    "xelatex -shell-escape -interaction nonstopmode %f"
    "rm -f %o/%b*.vrb")) ;; for multiple passes

;; Stop org from keep the tables centered
(setq org-latex-tables-centered nil)
(setq org-latex-listings 'minted)

(defvar en-article "
\\documentclass{scrartcl}
\\usepackage{hyperref}
\\usepackage{color}
\\usepackage[hyperref,x11names,usenames,dvipsnames]{xcolor}
\\hypersetup{colorlinks=true,linkcolor=BlueViolet}
\\usepackage{minted}
\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
\\usepackage[center,pagestyles]{titlesec}
\\usepackage{indentfirst}
\\usepackage[export]{adjustbox}
\\usemintedstyle{emacs}
\\setlength{\\parskip}{0.5\\baselineskip}
\\setlength{\\parindent}{0em}
\\titleformat{\\section}{\\Large\\bfseries}{\\S\\,\\thesection}{1em}{}
\\titleformat{\\subsection}{\\large\\bfseries}{\\S\\,\\thesubsection}{1em}{}
\\titleformat{\\subsubsection}{\\bfseries}{$\\cdot$~\\,\\thesubsubsection}{0.5em}{}
\\newpagestyle{main}{
\\sethead{\\small\\S\\,\\thesection\\quad\\sectiontitle}{}{$\\cdot$~\\thepage~$\\cdot$}
\\setfoot{}{}{}\\headrule}
\\pagestyle{main}
")

(defvar en-beamer "
\\documentclass\[presentation\]\{beamer\}
\\usepackage{minted}
\\usemintedstyle{emacs}
\\AtBeginSection[]{\\begin{frame}<beamer>\\frametitle{Topic}\\tableofcontents[currentsection]\\end{frame}}
")

(defvar zh-preamble "
\\usepackage{xeCJK}
\\setCJKmainfont[BoldFont=Source Han Sans CN Bold, ItalicFont=Source Han Snas CN Normal]{Source Han Sans CN Regular}
\\setCJKmonofont[Scale=0.9]{Source Code Pro Regular}
\\setCJKfamilyfont{song}[BoldFont=Source Han Sans CN Bold]{Source Han Sans CN Regular}
\\setCJKfamilyfont{sf}[BoldFont=Source Han Sans CN Bold]{Source Han Sans CN Regular}
\\renewcommand{\\contentsname}{目录}
\\renewcommand{\\listfigurename}{插图目录}
\\renewcommand{\\listtablename}{表格目录}
\\renewcommand{\\refname}{参考文献}
\\renewcommand{\\abstractname}{摘要}
\\renewcommand{\\indexname}{索引}
\\renewcommand{\\tablename}{表}
\\renewcommand{\\figurename}{图}
")



(defvar cn-article
  (concat en-article zh-preamble))

(defvar cn-beamer
  (concat en-beamer zh-preamble))

(require 'ox-latex)
(require 'ox-beamer)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             `("article"
               ,en-article
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
              `("cn-article"
                ,cn-article
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
              `("beamer"
                ,en-beamer
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
              `("cn-beamer"
                ,cn-beamer
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(defadvice org-html-paragraph (before fsh-org-html-paragraph-advice
                                      (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (let ((fixed-contents)
        (orig-contents (ad-get-arg 1))
        (reg-han "[[:multibyte:]]"))
    (setq fixed-contents (replace-regexp-in-string
                          (concat "\\(" reg-han
                                  "\\) *\n *\\(" reg-han "\\)")
                          "\\1\\2" orig-contents))
    (ad-set-arg 1 fixed-contents)))

(provide 'wen-orgmode)
