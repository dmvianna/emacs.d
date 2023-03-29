;;; languages.el --- Summary
;;; Commentary:
;;; Programming language support -- LSP and individual languages.
;;; Code:

;;; LSP

(use-package lsp-mode
  ;; lsp does not define this variable by
  ;; default, so we have to set it here
  :custom
  (lsp-enable-snippet nil)
  :init
  (setq
        ;; give lsp enough memory
        read-process-output-max (* 1024 1024) ;; 1mb)
        ;; Probably overkill, but doesn't hurt.
        ;; LSP should use better deserialisation.
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
        lsp-use-plists t
        ;; Yes, I want to restart
        lsp-restart 'auto-restart
        lsp-enable-folding nil
        lsp-enable-snippet nil)
  :hook
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)
  :commands (lsp lsp-deferred)
  :bind-keymap ("s-l" . lsp-command-map))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-use-webkit nil)
  :config
  (progn
    (define-key lsp-ui-mode-map
       [remap haskell-mode-jump-to-def-or-tag] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map
       [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map
       [remap xref-find-references] #'lsp-ui-peek-find-references))
  :commands lsp-ui-mode)

(use-package company-lsp
  :after lsp-ui
  :config
  (setq lsp-completion-provider :capf))

;; eglot
(use-package jsonrpc) ;; hopefully this doesn't leak memory
(use-package eglot
  :after jsonrpc
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits t)
  :hook (eglot-managed-mode . (lambda () (put 'eglot-note 'flymake-overlay-control nil)
                               (put 'eglot-warning 'flymake-overlay-control nil)
                               (put 'eglot-error 'flymake-overlay-control nil)))
   :config
   (setq-default
    eglot-workspace-configuration
    '(haskell
      (formattingProvider "fourmolu"))))

(defun my-eglot-organize-imports ()
  "Organize imports in eglot."
  (interactive)
  (eglot-code-actions nil nil "source.organizeImports" t))
(add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
(add-hook 'before-save-hook 'eglot-format-buffer)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;;; languages

(use-package avro-mode
  :straight nil
  :custom
  (tab-width 4)
  :mode "\\.avdl$")

;; ini files
(use-package conf-mode
  :straight nil
  :mode "\\.ini\\'\\|\\.lock\\'\\|\\.service\\'\\|\\.desktop\\'")

;; csv files
(use-package csv-mode
  :mode "\\.csv\\'")

;; Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package dockerfile-mode)

;; elm
(use-package elm-mode
  :hook (elm-mode . lsp-deferred))

;; graphviz
(use-package graphviz-dot-mode
  :config (setq graphviz-dot-mode-indent-width 2)
  :hook (graphviz-dot-mode . company-mode))

;; Haskell
(load-file (concat user-emacs-directory "mixins/languages/haskell-config.el"))

;; JSON
(use-package json-mode
  :mode "\\.json\\'\\|\\.jshintrc\\'"
  :interpreter "json-mode"
  :hook (json-mode . lsp-deferred))

;; java
(use-package lsp-java
  :hook (java-mode . lsp-deferred))

;;; javascript & web
(load-file (concat user-emacs-directory "mixins/languages/web-config.el"))

;; Gherkin
(use-package pickle
  :mode "\\.feature\\'"
  :interpreter "pickle-mode")

;; Lisp
(use-package parinfer-rust-mode
  :custom
  (parinfer-rust-auto-download t)
  (electric-pair-local-mode -1)
  :hook
  (emacs-lisp-mode
   lisp-mode
   lisp-interaction-mode
   geiser-mode
   racket-mode)
  (before-save . delete-trailing-whitespace))

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
  :init
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  :hook (nix-mode . eglot-ensure)
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
  :custom (sh-basic-offset 2))

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
  :init
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-lsp")))
  :hook
  (terraform-mode . eglot-ensure))

(use-package udev-mode)

(provide 'languages)
;;; languages.el ends here
