;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; my-package.el

;; Copyright (C) 2015 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; el-get packages
;;;; for auto-complete
(el-get-bundle fuzzy)
(el-get-bundle popup)
(el-get-bundle pos-tip)

;;;; Buffer utils
(el-get-bundle popwin)
(el-get-bundle yascroll)
(el-get-bundle buffer-move)

;; https://github.com/Fuco1/smartparens
(el-get-bundle Fuco1/smartparens)

;;;; flycheck
(el-get-bundle flycheck)

;;;; JavaScript
(el-get-bundle sourcemap)
(el-get-bundle js2-mode)
(el-get-bundle prettier-js)

;;;; Misc modes
(el-get-bundle company-mode)
(el-get-bundle company-quickhelp)
(el-get-bundle web-mode)
(el-get-bundle review-mode)
(el-get-bundle markdown-mode)
(el-get-bundle graphviz-dot-mode)
(el-get-bundle vue-mode)
(el-get-bundle vue-html-mode)

;;;; Utils
(el-get-bundle multi-term)

;;;; Color
(el-get-bundle powerline)
(el-get-bundle solarized-theme)

;;;; Elixir
(el-get-bundle erlang-mode)
(el-get-bundle elixir-lang/emacs-elixir)
(el-get-bundle tonini/alchemist.el)
(el-get-bundle syohex/emacs-ac-alchemist)

;; Python
(el-get-bundle python-mode)
(el-get-bundle jedi)
(el-get-bundle python-django)
(el-get-bundle py-autopep8)

;; Scala
(el-get-bundle scala-mode)

(el-get-bundle kotlin-mode)
(el-get-bundle flycheck-kotlin)

;; Misc Programming languages
(el-get-bundle ac-php)
(el-get-bundle rust-mode)
(el-get-bundle go-mode)
(el-get-bundle go-autocomplete)
(el-get-bundle rjsx-mode)
(el-get-bundle ghc-mod)
(el-get-bundle groovy-mode)

;; infra
(el-get-bundle terraform-mode)
(el-get-bundle hcl-mode)
(el-get-bundle nginx-mode)
(el-get-bundle dockerfile-mode)
(el-get-bundle docker-compose-mode)
(el-get-bundle toml-mode)
(el-get-bundle vimrc-mode)

(provide 'my-package)
;; my-el-get.el ends here
