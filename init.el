;;; Package --- Summary
;;; Commentary:
;;; init file
;;; Code:

(require 'misc-config)
;; (use-package proxy-config)

;; Markdown
(use-package
  markdown-mode
  :ensure t
  :straight t
  :config
  :mode "\\.md$"
  :interpreter "markdown-mode"
  )

;; Flycheck -- global syntax check (needed for hlint)
(use-package
  flycheck
  :ensure t
  :straight t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  )

;; Company -- text completion
(use-package company
  :ensure t
  :straight t)

;; JSON
(use-package
  json-mode
  :ensure t
  :straight t
  :mode "\\.json\\'\\|\\.jshintrc\\'"
  :interpreter "json-mode"
  )

(use-package
  racket-mode
  :ensure t
  :straight t
  :mode "\\.rkt\\'"
  :interpreter "racket-mode")

;; Lisp
(use-package
  paredit
  :ensure t
  :straight t
  :hook
  ((emacs-lisp-mode . paredit-mode)
    (lisp-mode . paredit-mode)
    (lisp-interaction-mode . paredit-mode)
    (geiser-mode . paredit-mode)
    (racket-mode . paredit-mode)))

;; rainbow
;; rainbow-delimiters for elisp
(use-package
  rainbow-delimiters
  :ensure t
  :straight t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (geiser-mode . rainbow-delimiters-mode)))


(provide 'init)
;;; init ends here
