;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_rails.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for rails
;;;; Rinari
;;(require 'rinari)
;;;; rhtml-mode
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

(require 'rspec-mode)
(custom-set-variables '(rspec-use-rake-flag nil))
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*rspec-compilation*"))
   (save-selected-window
      (save-excursion
         (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*rspec-compilation*")
          (shrink-window (- h 10)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(provide 'init_rails)
;; init_rails.el ends here
