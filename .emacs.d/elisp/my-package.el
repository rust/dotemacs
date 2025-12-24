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
(el-get-bundle graphviz-dot-mode)

;;;; Color
(el-get-bundle solarized-theme)

;; infra

(provide 'my-package)
;; my-el-get.el ends here
