;;; 'wen-pinyin.el' --- Emacs Chinese pinyin input selection.
;;; Using 'chinese-pyim' as default input.
;;; Depends: 
(require 'chinese-pyim)

(defun wen-pyim-dicts()
  ;; Use 'pyim-dicts-manager' to manage dicts
  ;; set dicts
  (setq pyim-dicts (quote(
                          (:name "bigdict" :file "~/.emacs.d/pyim/dicts/pyim-bigdict.pyim"  :coding utf-8-unix :dict-type pinyin-dict)
                          (:name "chinese-area-name" :file "~/.emacs.d/pyim/dicts/chinese-area-name.pyim" :coding utf-8 :dict-type
                                 (quote dict))
                          (:name "computer" :file "~/.emacs.d/pyim/dicts/computer.pyim" :coding utf-8 :dict-type
                                 (quote dict))
                          (:name "internet-words" :file "~/.emacs.d/pyim/dicts/internet-words.pyim" :coding utf-8 :dict-type
                                 (quote dict))
                          (:name "movie" :file "~/.emacs.d/pyim/dicts/movie.pyim" :coding utf-8 :dict-type
                                 (quote dict))
                          (:name "zhongyi" :file "~/.emacs.d/pyim/dicts/zhongyi.pyim" :coding utf-8 :dict-type
                                 (quote dict))
                          (:name "guessdict" :file "~/.emacs.d/pyim/dicts/pyim-guessdict.gpyim" :coding utf-8-unix :dict-type guess-dict)
                          )))
  )

;; 搜狗细胞词库转换为 pyim 词库
;; (setq pyim-dicts-manager-scel2pyim-command "~/.emacs.d/bin/scel2pyim")
;; 词语联想功能
(setq pyim-enable-words-predict '(pinyin-similar pinyin-shouzimu guess-words))
;; (setq pyim-enable-words-predict nil)
(wen-pyim-dicts)
(setq default-input-method "chinese-pyim")
;; default key 'C-\'
(global-set-key (kbd "C-<SPC>") 'toggle-input-method)
;; 切换全角标点与半角标点
(global-set-key (kbd "C-;") 'pyim-toggle-full-width-punctuation)
;; 设置光标跟随
;; (setq pyim-use-tooltip t)

(provide 'wen-pinyin)
