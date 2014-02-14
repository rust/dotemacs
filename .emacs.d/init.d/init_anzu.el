;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_anzu.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anzu
(require 'anzu)

(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-use-mimego t)
 '(anzu-replace-to-string-separator " => "))

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(provide 'init_anzu)
;; init_anzu.el ends here
