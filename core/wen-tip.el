;;; 'wen-tip.el' --- Emacs Wen tip.

(defvar wen-tips
  '("Press <C-x C-r> to open recent files."
    "Press <C-x C-z> to open completions from all backends."
    "Press <C-:> to open helm completion in company mode."
    "Press <M-g w> to jump the input char in line."
    "Press <M-g f> to jump the input char in file."
    "Press <M-g g> to jump the specail line."
    "Press <M-g C-y> to copy the specail line."
    "Press <M-g C-k> to move the special line."
    "Press <M-g M-y> to copy the special region."
    "Press <C-S-c C-S-c> to edit the marked lines into multi cursors."
    "Press <C-\>> to mark the next line like this."
    "Press <C-\<> to mark the previous line like this."
    "Press <C-c C-s> to skip and mark next line like this."
    "Press <C-=> to expand region."
    "Press <M-y> to list kill ring."
    "Press <M-s w> to save word at point."
    "Press <M-s s> to save sexp at point."
    "Press <M-s l> to save list at point."
    "Press <M-s d> to save current function."
    "Press <M-s D> to save current function name."
    "Press <M-s b> to save buffer file name or default directory."
    "Press <M-s @> to append selection to previous kill and exit."
    "Press <M-s C-w> to kill selection and exit."
    "Press <M-s +> to expand selection."
    "Press <M-s -> to shrink slection."
    "Press <M-s 0> to shrink the selection to the inititial size."
    "Press <M-s C-SPC> to turn slection into an active region."
    "Press <M-s C-g> to abort it."
    "Press <M-;> to comment or uncomment special lines."
    "Press <M-w> to kill whole line or marked region."
    "Press <C-c o> to open a line."
    "Press <C-c O> to open a line at above."
    "Press <C-c dd> to kill whole special lines."
    "Press <C-c x> to kill whole word."
    "Press <M-%> to replace in query mode."
    "Press <C-M-%> to regexp replace in query mode."
    "Press <C-x g> to show current git repo status."
    "Press <C-x v g> to toggle git gutter."
    "Press <C-x v => to pop gutter hunk."
    "Press <C-x v p> to jump previous gutter hunk."
    "Press <C-x v n> to jump next gutter hunk."
    "Press <C-x v s> to stage current hunk."
    "Press <C-x v r> to revert current hunk."
    "Press <C-x c g> to grep file by helm."
    "Press <C-x c o> to search input key by helm-occur."
    "Press <M-?> to search input key by helm-ag."
    "Press <C-\> to toggle default input method."
    "Press <C-M-;> to toggle pinyin full or width punctuation."
    "Press <C-c M-t> to open multi term."
    "Press <C-c M-e> to send ESC in multi term."
    "Press <C-c M-[> to jump previous multi term."
    "Press <C-c M-]> to jump next multi term."
    "Press <M-p> to jump window."
    "Press <M-p x> delete window in ace."
    "Press <M-p m> to swap window in ace."
    "Press <M-p n> to select the previous window."
    "Press <M-p v> to split window in vert mode."
    "Press <M-p b> to split window in horiz mode."
    "Press <M-p i> to maximize window."
    "Press <M-p o> to delete all other windows."
    "Press <C-c l> to store org link."
    "Press <C-c a> to open org agenda."
    "Press <C-c c> to open org capture."
    "Press <C-c b> to switch org buffer."
    "Press <C-c s g> to search in Google."
    "Press <C-c S-\\> to call youdao dict."
    "Access the official Emacs manual by pressing <C-h r>."
    "Visit the EmacsWiki at http://emacswiki.org to find out even more about Emacs."))

(defun wen-tip-of-the-day ()
  "Display a random entry from `wen-tips'."
  (interactive)
  (when (and wen-tips (not (window-minibuffer-p)))
    ;; pick a new random seed
    (random t)
    (message
     (concat "Wen tip: " (nth (random (length wen-tips)) wen-tips)))))

(defun wen-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(provide 'wen-tip)
