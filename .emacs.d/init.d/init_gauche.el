;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_gauche.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gauche
(modify-coding-system-alist 'process "gosh" '(utf-8 . utf-8))
(cond ((or mac-p ns-p)
       (setq scheme-program-name "/opt/local/bin/gosh -i"))
      (linux-p
       (setq scheme-program-name "gosh -i")))
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferir Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

(provide 'init_gauche)
;; init_gauche.el ends here