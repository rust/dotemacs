;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_elscreen.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elscreen
;; elscreen
(require 'elscreen)
(require 'elscreen-gf)
(require 'elscreen-howm)
(require 'elscreen-w3m)
(setq elscreen-prefix-key "\C-z")
(global-set-key (kbd "C-z SPC") 'elscreen-next)
(global-set-key (kbd "C-:") 'elscreen-next)
(global-set-key (kbd "C-z DEL") 'elscreen-previous)
(global-set-key (kbd "C-;") 'elscreen-previous)

(provide 'init_elscreen)
;; init_elscreen.el ends here