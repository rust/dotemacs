;;; init_yaml.el --- YAML設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; YAMLファイル編集のための設定。

;;; Code:

;; yaml
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (setq comment-start "#")
               (setq comment-start-skip "\\(^\\s-*\\|\\=\\s-*\\)#+ *")
               (setq comment-end-skip "$")
               (set (make-local-variable 'comment-style) 'indent) )))

(provide 'init_yaml)
;; init_yaml.el ends here
