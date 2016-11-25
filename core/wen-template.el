;;; 'wen-template.el': Emacs file template selection

;; C++
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
     '("Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       "\n */" > \n \n
       "#include <iostream>" \n \n
       "using namespace std;" \n \n
       "main()" \n
       "{" \n
       > _ \n
       "}" > \n)))

;; C
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.c\\'" . "C skeleton")
     '(
       "Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       "\n */" > \n \n
       "#include <stdio.h>" \n
       "#include \""
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))
       ".h\"" \n \n
       "int" \n
       "main(int argc, char *argv[])" \n
       "{" > \n
       > _ \n
       "}" > \n)))

;; C/C++ header
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.h\\'" . "C/C++ skeleton")
     '(
       "Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       " */" > \n \n
       "#ifndef __XX_H__" \n
       "#define __XX_H__" \n \n
       "#endif" \n)))

;; ruby
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.rb\\'" . "Ruby skeleton")
     '(
       "Short description: "
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n ###\n\n"
       "#!/usr/bin/ruby -w" \n
       "#-*-coding: utf-8 -*-" \n \n)))

;; python
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.py\\'" . "Python skeleton")
     '(
       "Short description: "
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n ###\n\n"
       "#!/usr/bin/env python3" \n
       "#-*-coding: utf-8 -*-" \n \n)))

;; shell
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.sh\\'" . "Shell skeleton")
     '(
       "Short description: "
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n ###\n\n"
       "#!/bin/bash" \n \n)))


(provide 'wen-template)
