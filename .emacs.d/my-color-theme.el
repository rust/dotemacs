(defun my-color-theme ()
  "Color theme by Shin-ichiro OGAWA, created 2008-11-05."
  (interactive)
  (color-theme-install
   '(my-color-theme
     ((background-color . "unspecified-bg")
      (background-mode . light)
      (foreground-color . "unspecified-fg"))
     ((align-highlight-change-face . highlight)
      (align-highlight-nochange-face . secondary-selection)
      (apropos-keybinding-face . underline)
      (apropos-label-face . italic)
      (apropos-match-face . match)
      (apropos-property-face . bold-italic)
      (apropos-symbol-face . bold)
      (compilation-message-face . underline)
      (ispell-highlight-face . isearch)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . match)
      (rmail-highlight-face . rmail-highlight)
      (snippet-bound-face . bold)
      (snippet-field-face . highlight)
      (tags-tag-face . default)
      (vc-annotate-very-old-color . "#3F3FFF")
      (view-highlight-face . highlight)
      (whitespace-display-spaces-in-color . t)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "unspecified-bg" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :family "default"))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (nil))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (button ((t (:underline t))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "brightblue"))))
     (compilation-column-number ((t (:bold t :foreground "color-34" :weight extra-bold))))
     (compilation-error ((t (:bold t :foreground "Red1" :weight bold))))
     (compilation-info ((t (:bold t :foreground "Green3" :weight bold))))
     (compilation-line-number ((t (:foreground "color-226"))))
     (compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
     (completions-common-part ((t (:stipple nil :background "unspecified-bg" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :family "default"))))
     (completions-first-difference ((t (:bold t :weight bold))))
     (cursor ((t (nil))))
     (custom-button ((t (nil))))
     (custom-button-mouse ((t (nil))))
     (custom-button-pressed ((t (nil))))
     (custom-button-pressed-unraised ((t (:foreground "magenta4" :underline t))))
     (custom-button-unraised ((t (:underline t))))
     (custom-changed ((t (:background "blue1" :foreground "white"))))
     (custom-comment ((t (:background "yellow3" :foreground "black"))))
     (custom-comment-tag ((t (:foreground "brightblue"))))
     (custom-documentation ((t (nil))))
     (custom-face-tag ((t (:bold t :weight bold :height 1.2 :family "helv"))))
     (custom-group-tag ((t (:bold t :foreground "blue1" :weight bold :height 1.2))))
     (custom-group-tag-1 ((t (:bold t :foreground "red1" :weight bold :height 1.2 :family "helv"))))
     (custom-invalid ((t (:background "red1" :foreground "yellow1"))))
     (custom-link ((t (:foreground "blue1" :underline t))))
     (custom-modified ((t (:background "blue1" :foreground "white"))))
     (custom-rogue ((t (:background "black" :foreground "pink"))))
     (custom-saved ((t (:underline t))))
     (custom-set ((t (:background "white" :foreground "blue1"))))
     (custom-state ((t (:foreground "dark green"))))
     (custom-themed ((t (:background "blue1" :foreground "white"))))
     (custom-variable-button ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag ((t (:bold t :foreground "blue1" :weight bold :height 1.2 :family "helv"))))
     (erb-comment-delim-face ((t (:bold t :foreground "red" :weight bold))))
     (erb-comment-face ((t (:bold t :background "color-237" :foreground "red" :weight bold))))
     (erb-delim-face ((t (nil))))
     (erb-exec-delim-face ((t (:bold t :weight bold))))
     (erb-exec-face ((t (:background "color-237"))))
     (erb-face ((t (:background "color-237"))))
     (erb-out-delim-face ((t (:bold t :foreground "color-82" :weight bold))))
     (erb-out-face ((t (:background "color-237"))))
     (escape-glyph ((t (:foreground "brown"))))
     (file-name-shadow ((t (:foreground "color-242"))))
     (fixed-pitch ((t (:family "courier"))))
     (flymake-errline ((t (:bold t :background "color-166" :weight bold))))
     (flymake-warnline ((t (:bold t :background "color-141" :weight bold))))
     (flyspell-duplicate ((t (:bold t :foreground "Gold3" :underline t :weight bold))))
     (flyspell-incorrect ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))
     (font-lock-builtin-face ((t (:foreground "Orchid"))))
     (font-lock-comment-delimiter-face ((t (:foreground "red"))))
     (font-lock-comment-face ((t (:foreground "red"))))
     (font-lock-constant-face ((t (:foreground "brightmagenta"))))
     (font-lock-doc-face ((t (:foreground "color-202"))))
     (font-lock-function-name-face ((t (:bold t :foreground "color-26" :weight bold))))
     (font-lock-keyword-face ((t (:bold t :foreground "brightcyan" :weight extra-bold))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground "Orchid"))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-string-face ((t (:foreground "color-202"))))
     (font-lock-type-face ((t (:bold t :foreground "color-34" :weight extra-bold))))
     (font-lock-variable-name-face ((t (:foreground "color-226"))))
     (font-lock-warning-face ((t (:bold t :foreground "Red1" :weight bold))))
     (fringe ((t (:background "grey95"))))
     (header-line ((t (:background "grey75" :foreground "black" :inverse-video nil :box (:line-width -1 :style released-button) :underline t))))
     (help-argument-name ((t (nil))))
     (highlight ((t (:background "darkseagreen2"))))
     (isearch ((t (:background "magenta3" :foreground "lightskyblue1"))))
     (italic ((t (:underline t))))
     (jaspace-highlight-eol-face ((t (:foreground "darkseagreen"))))
     (jaspace-highlight-jaspace-face ((t (:foreground "blue"))))
     (jaspace-highlight-tab-face ((t (:background "unspecified" :foreground "red" :strike-through nil :underline t))))
     (lazy-highlight ((t (:background "paleturquoise"))))
     (link ((t (:foreground "blue1" :underline t))))
     (link-visited ((t (:foreground "magenta4" :underline t))))
     (match ((t (:background "yellow"))))
     (menu ((t (nil))))
     (minibuf-isearch-comp-face ((t (:background "khaki" :underline t))))
     (minibuf-isearch-face ((t (:bold t :background "blue" :underline t :weight bold))))
     (minibuffer-prompt ((t (:foreground "brightblue"))))
     (mmm-cleanup-submode-face ((t (:background "Wheat"))))
     (mmm-code-submode-face ((t (:background "LightGray"))))
     (mmm-comment-submode-face ((t (:background "SkyBlue"))))
     (mmm-declaration-submode-face ((t (:background "Aquamarine"))))
     (mmm-default-submode-face ((t (:background "gray85"))))
     (mmm-delimiter-face ((t (nil))))
     (mmm-init-submode-face ((t (:background "Pink"))))
     (mmm-output-submode-face ((t (:background "Plum"))))
     (mmm-special-submode-face ((t (:background "MediumSpringGreen"))))
     (mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     (mode-line-buffer-id ((t (:bold t :weight bold))))
     (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
     (mode-line-inactive ((t (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light))))
     (mouse ((t (nil))))
     (next-error ((t (:background "color-55"))))
     (nobreak-space ((t (:foreground "brown" :underline t))))
     (nxml-attribute-colon-face ((t (:foreground "#257A25"))))
     (nxml-attribute-local-name-face ((t (:foreground "#257A25"))))
     (nxml-attribute-prefix-face ((t (:foreground "#257A25"))))
     (nxml-attribute-value-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-attribute-value-face ((t (:foreground "#3A3A7B"))))
     (nxml-cdata-section-CDATA-face ((t (:foreground "#257A25"))))
     (nxml-cdata-section-content-face ((t (nil))))
     (nxml-cdata-section-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-char-ref-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-char-ref-number-face ((t (:foreground "#9292C9"))))
     (nxml-comment-content-face ((t (:italic t :slant italic))))
     (nxml-comment-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-delimited-data-face ((t (:foreground "#3A3A7B"))))
     (nxml-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-element-colon-face ((t (:foreground "#257A25"))))
     (nxml-element-local-name-face ((t (:foreground "#257A25"))))
     (nxml-element-prefix-face ((t (:foreground "#257A25"))))
     (nxml-entity-ref-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-entity-ref-name-face ((t (:foreground "#9292C9"))))
     (nxml-glyph-face ((t (:background "light grey" :foreground "black" :slant normal :weight normal))))
     (nxml-hash-face ((t (:foreground "#257A25"))))
     (nxml-heading-face ((t (:bold t :weight bold))))
     (nxml-markup-declaration-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-name-face ((t (:foreground "#257A25"))))
     (nxml-namespace-attribute-colon-face ((t (:foreground "#257A25"))))
     (nxml-namespace-attribute-prefix-face ((t (:foreground "#257A25"))))
     (nxml-namespace-attribute-value-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-namespace-attribute-value-face ((t (:foreground "#3A3A7B"))))
     (nxml-namespace-attribute-xmlns-face ((t (:foreground "#257A25"))))
     (nxml-outline-active-indicator-face ((t (:stipple nil :background "unspecified-bg" :foreground "unspecified-fg" :inverse-video nil :box 1 :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :family "default"))))
     (nxml-outline-ellipsis-face ((t (:bold t :stipple nil :background "unspecified-bg" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 1 :width normal :family "default"))))
     (nxml-outline-indicator-face ((t (:stipple nil :background "unspecified-bg" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :family "default"))))
     (nxml-processing-instruction-content-face ((t (:foreground "#3A3A7B"))))
     (nxml-processing-instruction-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-processing-instruction-target-face ((t (:foreground "#257A25"))))
     (nxml-prolog-keyword-face ((t (:foreground "#257A25"))))
     (nxml-prolog-literal-content-face ((t (:foreground "#3A3A7B"))))
     (nxml-prolog-literal-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-ref-face ((t (:foreground "#9292C9"))))
     (nxml-tag-delimiter-face ((t (:foreground "#9292C9"))))
     (nxml-tag-slash-face ((t (:foreground "#257A25"))))
     (nxml-text-face ((t (nil))))
     (query-replace ((t (:background "magenta3" :foreground "lightskyblue1"))))
     (region ((t (:background "color-55"))))
     (scroll-bar ((t (nil))))
     (secondary-selection ((t (:background "yellow1"))))
     (sgml-namespace ((t (:foreground "Orchid"))))
     (shadow ((t (:foreground "grey50"))))
     (show-paren-match ((t (:background "turquoise"))))
     (show-paren-mismatch ((t (:background "purple" :foreground "white"))))
     (speedbar-button-face ((t (:foreground "green4"))))
     (speedbar-directory-face ((t (:foreground "blue4"))))
     (speedbar-file-face ((t (:foreground "cyan4"))))
     (speedbar-highlight-face ((t (:background "green"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
     (speedbar-tag-face ((t (:foreground "brown"))))
     (tmm-inactive ((t (:foreground "grey50"))))
     (tool-bar ((t (:foreground "black" :box (:line-width 1 :style released-button)))))
     (tooltip ((t (:background "lightyellow" :foreground "black" :family "helv"))))
     (trailing-whitespace ((t (:background "unspecified" :foreground "red" :strike-through nil :underline t))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "helv"))))
     (vertical-border ((t (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light))))
     (wb-line-number-face ((t (:foreground "hotpink"))))
     (wb-line-number-scroll-bar-face ((t (:background "hotpink" :foreground "black"))))
     (whitespace-highlight ((t (:background "green1"))))
     (widget-button ((t (:bold t :weight bold))))
     (widget-button-pressed ((t (:foreground "red1"))))
     (widget-documentation ((t (:foreground "dark green"))))
     (widget-field ((t (:background "yellow3" :foreground "black"))))
     (widget-inactive ((t (:foreground "grey50"))))
     (widget-single-line-field ((t (:background "green3" :foreground "black"))))
     (yaml-tab-face ((t (:bold t :background "red" :foreground "red" :weight bold)))))))
