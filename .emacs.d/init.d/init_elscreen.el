;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_elscreen.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elscreen
(require 'elscreen)
;; prefix-setting
(elscreen-set-prefix-key "\C-z")
(global-set-key (kbd "C-z SPC") 'elscreen-next)
(global-set-key (kbd "C-z DEL") 'elscreen-previous)
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
