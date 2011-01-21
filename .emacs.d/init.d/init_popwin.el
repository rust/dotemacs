;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_popwin.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)

(provide 'init_popwin)
;; init_mail.el ends here
