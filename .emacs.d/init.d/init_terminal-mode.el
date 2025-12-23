;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_terminal-mode.el

;; Copyright (C) 2025 Shinichiro OGAWA
;;   Author  : Shinichiro OGAWA <rust.stnard@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization in Terminal

(tool-bar-mode -1)
(menu-bar-mode -1)

(global-set-key (kbd "C-m") 'newline-and-indent)

(load-theme 'solarized-light t)

(provide 'init_terminal-mode)
;; init_terminal-mode.el ends here
