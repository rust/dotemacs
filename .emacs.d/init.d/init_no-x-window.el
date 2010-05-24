;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_no-x-window.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; メニューバーを消す
(menu-bar-mode nil)

;; 256 colors
(load "emacs21-256color-hack.el")
(require 'color-theme)
(load "my-color-theme")
(my-color-theme)

;; wb-line-number
(require 'wb-line-number)
(wb-line-number-toggle)
(set-scroll-bar-mode nil)
(setq wb-line-number-scroll-bar t)

(provide 'init_no-x-window)
;; init_no-x-window.el ends here