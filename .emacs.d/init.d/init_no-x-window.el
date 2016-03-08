;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_no-x-window.el

;; Copyright (C) 2015 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization of non X

;; メニューバーを消す
(menu-bar-mode -1)

(global-set-key (kbd "C-m") 'newline-and-indent)

(provide 'init_no-x-window)
;; init_no-x-window.el ends here
