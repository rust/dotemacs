;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_org.el

;; Copyright (C) 2019 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-mode
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-M-^" . (lambda() (interactive) (show-org-buffer "notes.org"))))
  :preface
  (defun show-org-buffer (file)
    "Show an org-file FILE on the current buffer."
    (interactive)
    (if (get-buffer file)
        (let ((buffer (get-buffer file)))
          (switch-to-buffer buffer)
          (message "%s" file))
      (find-file (concat "~/Works/org/" file))))
  :custom
  (org-directory "~/Works/org")
  (org-default-notes-file "notes.org")
  (org-capture-templates
   '(("n" "Note" entry (file+headline "~/Works/org/notes.org" "Notes")
      "* %?\nEntered on %U\n %i\n %a")
     ))
  (org-startup-with-inline-image t)
  (my-org-agenda-dir "~/org")
  (org-agenda-file (list my-org-agenda-dir))
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

(use-package org-bullet
  :config
  (org-mode-hook . (lambda() (org-bullet-mode 1))))

(provide 'init_org)
;; init_org.el ends here
