;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_multi-term.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term)
(setq multi-term-program shell-file-name)

(add-hook 'term-mode-hook
          '(lambda ()
             ;; C-h を term 内文字削除にする
             (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
             ;; C-y を term 内ペーストにする
             (define-key term-raw-map (kbd "C-y") 'term-paste)
             ))

(global-set-key (kbd "C-c t") '(lambda ()
                                 (interactive)
                                 (if (get-buffer "*terminal<1>*")
                                     (switch-to-buffer "*terminal<1>*")
                                   (multi-term))))

(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)

(provide 'init_multi-term)
;; init_multi-term.el ends here
