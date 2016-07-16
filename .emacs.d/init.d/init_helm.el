;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_helm.el

;; Copyright (C) 2013 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm-config)
(require 'helm-command)
(require 'helm-descbinds)

(global-set-key "\C-xb" 'helm-mini)
;;(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "<menu>") 'helm-M-x)


(helm-mode)
(helm-descbinds-mode)

(setq helm-ff-auto-update-initial-value t)

(provide 'init_helm)
;; init_helm.el ends here
