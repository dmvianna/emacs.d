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
  :hook (flycheck-pycheckers-setup)
  )

(use-package flycheck-pycheckers
  :ensure t
  :straight t)

;; Company -- text completion
(use-package company
  :ensure t
  :straight t)

;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :straight t
  :commands lsp-ui-mode
  :hook (haskell-lsp-ui
         python-lsp-ui)
  :config
  (custom-set-variables
   (lsp-ui-sideline-show-diagnostics t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-sideline-show-code-actions t)
   (lsp-ui-sideline-update-mode t)
   (lsp-ui-peek-enable t)
   (lsp-ui-peek-show-directory t)
   (lsp-ui-doc-enable t)
   (lsp-ui-doc-position 'bottom)
   (lsp-ui-doc-show-with-cursor t)
   (lsp-ui-doc-show-with-mouse t)
   (lsp-ui-imenu-auto-refresh t)
   (lsp-ui-imenu-window-width 40)
   ))

(use-package lsp-haskell
  :ensure t
  :straight t
  :hook (haskell-mode haskell-config)
  )

(use-package lsp-python-ms
  :ensure t
  :straight t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

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
