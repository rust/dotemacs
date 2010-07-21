;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_anything.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything
;; (require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
;; (define-key global-map "\C-xb" 'anything)
;; (global-set-key (kbd "C-x b") 'anything)
(global-set-key "\C-xb" 'anything)

;; descbinds-anything
(require 'descbinds-anything)
(descbinds-anything-install)
;; ;; ac-complete.el
;; (require 'ac-anything)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything)

(provide 'init_anything)
;; init_anything.el ends here
