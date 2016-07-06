;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_scala.el

;; Copyright (C) 2015 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala

;; scala-mode2
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\.sbt$" . scala-mode))

;; ensime
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'init_scala)
;; init_scala.el ends here
