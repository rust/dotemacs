;;; init_rails.el --- Rails開発環境設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Ruby on Rails開発のための設定。
;; RHTMLモードなどのテンプレート関連の設定を含む。

;;; Code:

;; for rails
;;;; rhtml-mode
(use-package rhtml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode)))

(use-package rspec-mode
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'rspec-mode)
  (add-hook 'enh-ruby-mode-hook 'rspec-mode)

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
  (add-hook 'compilation-mode-hook 'my-compilation-hook))

(provide 'init_rails)
;; init_rails.el ends here
