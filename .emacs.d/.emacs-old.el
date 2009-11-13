;; multi-term
(require 'multi-term)
(setq multi-term-program "/usr/bin/zsh")
(global-set-key "\C-t" 'multi-term)
(setq multi-term-dedicated-window-height 10)
(setq multi-term-dedicated-max-window-height 20)
(setq term-unbind-key-list (quote ("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")))
