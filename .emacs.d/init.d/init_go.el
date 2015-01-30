;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_go.el

;; Copyright (C) 2014 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run gofmt before saving
(add-hook 'before-save-hook 'gofmt-before-save)

(add-to-list 'exec-path (expand-file-name "~/Works/golang/bin"))

;; % go get code.google.com/p/rog-go/exp/cmd/godef
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

;; autocomplete
;; % go get -u github.com/nsf/gocode
 (add-to-list 'load-path (expand-file-name "~/Works/golang/src/github.com/nsf/gocode/emacs"))
(require 'go-autocomplete)
(require 'auto-complete-config)

;; go-flymake
;; % go get -u github.com/dougm/goflymake
(add-to-list 'load-path (expand-file-name "~/Works/golang/src/github.com/dougm/goflymake"))
(require 'go-flymake)

(provide 'init_go)
;; init_go.el ends here
