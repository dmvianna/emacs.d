;;; Package --- Summary
;;; Commentary:
;;; init file
;;; Code:

(require 'misc-config)
;; (use-package proxy-config)

;; Flycheck -- global syntax check (needed for hlint)
(use-package flycheck
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

;; LSP
(use-package eglot
  :ensure t
  :straight t)

;; JSON
(use-package json-mode
  :ensure t
  :straight t
  :mode "\\.json\\'\\|\\.jshintrc\\'"
  :interpreter "json-mode"
  )

;; Gherkin
(use-package pickle
  :ensure t
  :straight t
  :mode "\\.feature\\'"
  :interpreter "pickle-mode")

;; Lisp
(use-package paredit
  :ensure t
  :straight t
  :hook
  ((emacs-lisp-mode . paredit-mode)
    (lisp-mode . paredit-mode)
    (lisp-interaction-mode . paredit-mode)
    (geiser-mode . paredit-mode)
    (racket-mode . paredit-mode)))

;; Markdown
(use-package markdown-mode
  :ensure t
  :straight t
  :config
  :mode "\\.md$"
  :interpreter "markdown-mode"
  )

;; Python

(use-package flycheck-pycheckers
  :ensure t
  :straight t
  :hook (flycheck-mode . flycheck-pycheckers-setup))

(use-package python-black
  :ensure t
  :straight t
  :hook (python-mode . python-black))

;; Racket
(use-package racket-mode
  :ensure t
  :straight t
  :mode "\\.rkt\\'"
  :interpreter "racket-mode")

;; rainbow
;; rainbow-delimiters for elisp
(use-package rainbow-delimiters
  :ensure t
  :straight t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (geiser-mode . rainbow-delimiters-mode)))

(use-package sh-script
  :ensure nil
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("zshrc\\'" . sh-mode)
         ("zshenv\\'" . sh-mode))
  :bind (:map sh-mode-map
              ("C-c C-e" . sh-execute-region))
  )


(provide 'init)
;;; init ends here
