;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_elscreen.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elscreen
(require 'elscreen)
(require 'elscreen-gf)
(require 'elscreen-howm)
(require 'elscreen-w3m)
;; coexist for screen
(elscreen-set-prefix-key "\C-c\C-t")
(global-set-key (kbd "C-c C-t SPC") 'elscreen-next)
(global-set-key (kbd "C-c C-t DEL") 'elscreen-previous)
;; compatibility for MacOS X
(global-set-key "\M-t" 'elscreen-create)
(global-set-key "\M-T" 'elscreen-clone)
(global-set-key "\M-}" 'elscreen-next)
(global-set-key "\M-{" 'elscreen-previous)
(global-set-key [(s t)] 'elscreen-create)
(global-set-key [(s w)] 'elscreen-kill)
(global-set-key [(s })] 'elscreen-next)
(global-set-key [(s {)] 'elscreen-previous)
(global-set-key [(C-tab)] 'elscreen-next)
(global-set-key [(C-S-iso-lefttab)] 'elscreen-previous)

(provide 'init_elscreen)
;; init_elscreen.el ends here