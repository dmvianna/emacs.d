;;; Package --- Summary
;;; Commentary:
;;; init file
;;; Code:

(require 'misc-config)
;; (use-package proxy-config)

;; Flycheck -- global syntax check (needed for hlint)
;; we need to upgrade the inbuilt flymake version so
;; that packages that require it don't fail
(use-package flymake
  :ensure t
  :straight t)

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

(use-package yasnippet
  :ensure t
  :straight t)

(use-package which-key
  :ensure t
  :straight t
  :config
  (which-key-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :straight t
  :init (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom)
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :straight t
  :after lsp-ui)

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

;; Haskell

(use-package haskell-mode
  :ensure t
  :straight t
  :config
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
)

(use-package haskell-interactive-mode
  :hook
  (haskell-mode-hook . interactive-haskell-mode))

(use-package haskell-process
  :custom
  (haskell-process-type 'stack-ghci)
  (haskell-indent-spaces 2))

(use-package lsp-haskell
  :ensure t
  :straight t
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp)
  :config
  (add-to-list 'lsp-enabled-clients 'lsp-haskell))

(use-package ormolu
  :ensure t
  :straight t
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind (:map haskell-mode-map
              ("C-c r" . ormolu-format-buffer)))

;; Dhall
(use-package dhall-mode
  :ensure t
  :straight t
  :mode "\\.dhall\\'")

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

(use-package flycheck-mypy
  :ensure t
  :straight t)

(use-package elpy
  :ensure t
  :straight t
  :bind
  (:map elpy-mode-map
        ("C-M-n" . elpy-nav-forward-block)
        ("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode)
         (elpy-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             '((elpy-company-backend :with company-yasnippet))))))
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'system)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-timeout 2))

;; (use-package lsp-pyright
;;   :ensure t
;;   :straight t
;;   :config
;;   (add-to-list 'lsp-enabled-clients 'lsp-pyright)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))

;; (use-package python-mode
;;   :interpreter ("python" . python-mode))

(use-package py-isort
  :ensure t
  :straight t
  :after python
  :hook (before-save . py-isort-before-save))

(use-package python-black
  :ensure t
  :straight t
  :hook (python-mode . python-black))

;; (use-package flycheck-pycheckers
;;   :ensure t
;;   :straight t
;;   :custom
;;   (flycheck-pycheckers-checkers '(mypy3 pyflakes))
;;   ;; :config (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
;;   ;; :hook (flycheck-mode . flycheck-pycheckers-setup)
;;   )

;; (use-package flycheck-pyre
;;   :ensure t
;;   :straight t)

;; (use-package lsp-pyre
;;   :ensure t
;;   :straight t
;;   :config
;;   (add-to-list 'lsp-enabled-clients 'lsp-pyre)
;;   :hook (python-mode . lsp))


;; (use-package lsp-pyright
;;   :ensure t
;;   :straight t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          ((lsp)))))

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
