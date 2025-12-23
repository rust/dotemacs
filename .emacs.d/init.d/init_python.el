;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_python.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
;; pymacs
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "~/.emacs.d/scripts"))
;; python-mode, pycomplete
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(add-hook 'python-mode-hook '(lambda ()
                               (require 'pycomplete)
                               ))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(require 'python-django)

(provide 'init_python)
;; init_python.el ends here
