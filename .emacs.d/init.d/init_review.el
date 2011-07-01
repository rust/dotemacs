;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_review.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review
(require 'review-mode)

(add-to-list 'auto-mode-alist '("\\.re$" . review-mode))
(add-hook 'review-mode-hook
          '(lambda()
             (setq comment-start "#@#")
             (setq mode-name "著者")
             (setq tab-width 8)
             (setq indent-tabs-mode t)))

(provide 'init_review)
;; init_review.el ends here
