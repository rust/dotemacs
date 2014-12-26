;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_markdown.el

;; Copyright (C) 2014 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-mode
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq markdown-command "/usr/local/bin/multimarkdown")

(add-hook 'visual-line-mode-hook
          '(lambda()
             (setq word-wrap nil)))

(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit org-level-1 markdown-header-face))))
 '(markdown-header-face-2 ((t (:inherit org-level-2 markdown-header-face))))
 '(markdown-header-face-3 ((t (:inherit org-level-3 markdown-header-face))))
 '(markdown-header-face-4 ((t (:inherit org-level-4 markdown-header-face))))
 '(markdown-header-face-5 ((t (:inherit org-level-5 markdown-header-face))))
 '(markdown-header-face-6 ((t (:inherit org-level-6 markdown-header-face)))))

(provide 'init_markdown)
;; init_markdown.el ends here
