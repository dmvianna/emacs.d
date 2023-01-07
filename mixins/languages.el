;;; languages.el --- Summary
;;; Commentary:
;;; Programming language support -- LSP and individual languages.
;;; Code:

;;; LSP
(use-package lsp-mode
  ;; lsp does not define this variable by
  ;; default, so we have to set it here
  :custom (lsp-enable-snippet nil)
  :init
  (setq lsp-keymap-prefix "s-l")
  ;; give lsp enough memory
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 80000000)
  :hook
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-use-webkit nil)
  :commands lsp-ui-mode)

(use-package company-lsp
  :after lsp-ui
  :config
  (setq lsp-completion-provider :capf))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;;; languages

(use-package avro-mode
  :straight nil
  :custom
  (tab-width 4)
  :mode "\\.avdl$")

;; ini files
(use-package conf-mode
  :straight nil
  :mode "\\.ini\\'\\|\\.lock\\'\\|\\.service\\'")

;; csv files
(use-package csv-mode
  :mode "\\.csv\\'")

;; Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package dockerfile-mode)

;; graphviz
(use-package graphviz-dot-mode
  :config (setq graphviz-dot-mode-indent-width 2))
(use-package company-graphviz-dot
  :straight nil)

;; Haskell
(load-file (concat user-emacs-directory "mixins/languages/haskell-config.el"))

;; JSON
(use-package json-mode
  :mode "\\.json\\'\\|\\.jshintrc\\'"
  :interpreter "json-mode"
  )

;; java
(use-package lsp-java
  :hook (java-mode . lsp))

;;; javascript & web
(load-file (concat user-emacs-directory "mixins/languages/web-config.el"))

;; Gherkin
(use-package pickle
  :mode "\\.feature\\'"
  :interpreter "pickle-mode")

;; Lisp
(use-package parinfer-rust-mode
  :init
  (setq parinfer-rust-auto-download t)
  :hook
  ((emacs-lisp-mode . parinfer-mode)
   (lisp-mode . parinfer-mode)
   (lisp-interaction-mode . parinfer-mode)
   (geiser-mode . parinfer-mode)
   (racket-mode . parinfer-mode)
   (before-save . delete-trailing-whitespace)))

;; Markdown
(use-package markdown-mode
  :hook
  (markdown-mode . display-line-numbers-mode)
  :mode "\\.md$"
  :interpreter "markdown-mode"
  :hook flyspell)

;; Python
(load-file (concat user-emacs-directory "mixins/languages/python-config.el"))

;; nix
(use-package nix-mode
  :after lsp
  :init
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  :mode "\\.nix\\'")

;; Racket
(use-package racket-mode
  :mode "\\.rkt\\'"
  :interpreter "racket-mode")

;; scala
(load-file (concat user-emacs-directory "mixins/languages/scala-config.el"))

(use-package sh-script
  :ensure nil
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("zshrc\\'" . sh-mode)
         ("zshenv\\'" . sh-mode))
  :bind (:map sh-mode-map
              ("C-c C-e" . sh-execute-region))
  :custom (sh-basic-offset 2)
  )

(use-package theta-mode
  :straight (theta-mode
             :type git
             :host github
             :repo "target/theta-idl"
             :branch "stage"
             :files ("emacs/theta-mode.el"))
  :mode ("\\.theta\\'" . theta-mode))

(use-package yaml-mode)

(use-package terraform-mode
  :hook
  (terraform-mode . lsp))

(provide 'languages)
;;; languages.el ends here