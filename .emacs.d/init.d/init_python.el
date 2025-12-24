;;; init_python.el --- Python設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Python開発環境の設定。
;; python-modeとDjangoサポートの設定を含む。

;;; Code:

;; python

;; python-mode, pycomplete
(use-package python-mode
  :ensure t)
(use-package python-django
  :ensure t)

(provide 'init_python)
;; init_python.el ends here
