;;; custom.el --- Emacsカスタマイズ設定  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacsのカスタマイズインターフェースで設定された値を保存するファイル。
;; このファイルは自動生成されるため、手動で編集しないこと。

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(anzu-use-mimego t)
 '(custom-safe-themes
   '("2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b"
     "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
     "0f9a1b7a0f1d09544668297c1f04e5a5452ae1f4cf69f11b125f4cff1d54783d"
     default))
 '(package-selected-packages nil)
 '(rspec-use-rake-flag nil)
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:inherit org-level-1 markdown-header-face))))
 '(markdown-header-face-2 ((t (:inherit org-level-2 markdown-header-face))))
 '(markdown-header-face-3 ((t (:inherit org-level-3 markdown-header-face))))
 '(markdown-header-face-4 ((t (:inherit org-level-4 markdown-header-face))))
 '(markdown-header-face-5 ((t (:inherit org-level-5 markdown-header-face))))
 '(markdown-header-face-6 ((t (:inherit org-level-6 markdown-header-face)))))
