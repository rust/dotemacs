;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_diff.el

;; Copyright (C) 2013 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff

(defun diff-mode-setup-faces ()
  (set-face-attribute 'diff-added-face nil
                      :foreground "green"
                      :background nil
                      :weight 'normal)
  (set-face-attribute 'diff-removed-face nil
                      :foreground "firebrick1"
                      :background nil
                      :weight 'normal)
  (set-face-attribute 'diff-refine-change nil
                      :background "gray12"
                      :weight 'extra-bold)
  (set-face-attribute 'diff-header-face nil
                      :background "midnight blue")
  (set-face-attribute 'diff-file-header-face nil
                      :foreground "DarkOrange1"
                      :background "midnight blue"
                      :weight 'extra-bold)
  (set-face-attribute 'diff-hunk-header-face nil
                      :foreground "deep sky blue"
                      :background "midnight blue"
                      :weight 'extra-bold)
  )
(add-hook 'diff-mode-hook 'diff-mode-setup-faces)

(provide 'init_diff)
;; init_diff.el ends here
