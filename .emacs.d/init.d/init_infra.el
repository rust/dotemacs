;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_infra.el

;; Copyright (C) 2016 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; infra

;; script-mode
(add-to-list 'auto-mode-alist '("\\.zsh" . shell-script-mode))

;; conf-mode
(add-to-list 'auto-mode-alist '("ssh/config"    . conf-mode))
(add-to-list 'auto-mode-alist '("\\*ssh_config" . conf-mode))

;; docker
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(provide 'init_infra)
;; init_infra.el ends here
