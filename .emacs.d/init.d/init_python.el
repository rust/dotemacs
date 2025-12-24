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
(use-package python-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
  (setq jedi:complete-on-dot t)
  (setq py-autopep8-options '("--max-line-length=200"))
  (setq flycheck-flake8-maximum-line-length 200)
  (py-autopep8-enable-on-save)
  (add-hook 'python-mode-hook '(lambda ()
                                 (require 'pycomplete)
                               ))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package python-django
  :ensure t)

(provide 'init_python)
;; init_python.el ends here
