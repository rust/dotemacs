;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_linux.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cask
(require 'cask)
(cask-initialize)

;; X or no-X
(cond
 (x-window-p (require 'init_x-window))
 (no-x-p (require 'init_no-x-window)))

(when lubuntu-p (require 'init_disable-mouse))

;; rdefs
(setq ar:command "~/bin/rdefs")

(provide 'init_linux)
;; init_linux.el ends here
