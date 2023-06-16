;;; org.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; org-mode customisations.
;;;
;;; Code:

(use-package org-pandoc-import
  :elpaca (:host github
                 :repo "tecosaur/org-pandoc-import"
                 :files ("*.el" "filters" "preprocessors")))

(use-package org-roam)

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Documents/Notes/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank)))
  :custom (org-download-method 'attach)
  :ensure-system-package wl-clipboard)

;;; execute scripts from org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))

(provide 'org-config.el)
;;; org-config.el ends here.
