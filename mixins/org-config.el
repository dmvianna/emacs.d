;;; org.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; org-mode customisations.
;;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Org mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org is loaded by other packages, so it must be loaded first lest we load
;; conflicting versions
(use-package org
  :elpaca (:host github :repo "bzg/org-mode")
  :mode ("\\.org\\'" . org-mode)
  :bind
  (:map org-mode-map
        ("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package org-pandoc-import
  :after org
  :elpaca (:host github
                 :repo "tecosaur/org-pandoc-import"
                 :files ("*.el" "filters" "preprocessors")))

(use-package org-roam
  :after org)

(use-package org-journal
  :after org
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
(use-package org-babel
  :elpaca (:host github :repo "taruti/org-babel")
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

;; rest
(use-package verb
  :after org)

(provide 'org-config.el)
;;; org-config.el ends here.
