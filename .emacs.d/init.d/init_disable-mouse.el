;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_disable-mouse.el

;; Copyright (C) 2012 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disabling all mouse actions

(global-unset-key [mouse-1])
(global-unset-key [down-mouse-1])
(global-unset-key [drag-mouse-1])
(global-unset-key [double-mouse-1])
(global-unset-key [double-drag-Mouse-1])
(global-unset-key [triple-mouse-1])
(global-unset-key [triple-drag-mouse-1])
(global-unset-key [\S-down-mouse-1])
(global-unset-key [\C-down-mouse-1])
(global-unset-key [\M-mouse-1])
(global-unset-key [\M-down-mouse-1])
(global-unset-key [\M-drag-mouse-1])
(global-unset-key [mouse-2])
(global-unset-key [mouse-3])
(global-unset-key [\S-mouse-3])
(global-unset-key [\S-down-mouse-3])
(global-unset-key [\C-down-mouse-3])
(global-unset-key [\M-mouse-3])

(provide 'init_disable-mouse)
;; init_disable-mouse.el ends here
