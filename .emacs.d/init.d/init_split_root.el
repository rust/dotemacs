;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_split_root.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'split-root)
(defvar split-root-window-height nil)
(defun display-buffer-function--split-root (buf &optional ignore)
  (let ((window (split-root-window split-root-window-height)))
    (set-window-buffer window buf)
    window))

(setq anything-display-function 'display-buffer-function--split-root)

(provide 'init_split_root)
;; init_mail.el ends here
