;;; languages.el --- Summary  -*- lexical-binding: t; -*-
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
   ;; easier than debugging right now https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
   lsp-enable-file-watchers nil
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
  :commands (lsp lsp-deferred)
  :bind-keymap ("s-l" . lsp-command-map)
  :hook (lsp-managed-mode . (lambda ()
                              (add-hook 'before-save-hook 'lsp-format-buffer nil t)
                              (add-hook 'before-save-hook 'lsp-organize-imports nil t))))

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

(use-package lsp-treemacs)

;; eglot

(use-package eglot
  :ensure nil
  :after eldoc
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits t)
  (eglot-sync-connect nil) ; don't block while connecting to server
  :hook (eglot-managed-mode . (lambda ()
                                (put 'eglot-note 'flymake-overlay-control nil)
                                (put 'eglot-warning 'flymake-overlay-control nil)
                                (put 'eglot-error 'flymake-overlay-control nil)
                                (add-hook 'before-save-hook
                                          'eglot-format-buffer nil t)))
  ;; (add-hook 'before-save-hook
  ;;           #'(lambda ()
  ;;               (eglot-code-action-organize-imports
  ;;                nil nil))
  ;; nil t)

  :init
  (setq-default
   eglot-ignored-server-capabilities
   '(workspace/didChangeWatchedFiles)))

;; ein
(use-package ein)
;; jupyter installation (with Rust repl)
;;
;; python -m venv .venv
;; source .venv/bin/activate
;; pip install jupyterlab
;; cargo install evcxr_jupyter
;; evcxr_jupyter --install

;;; languages

(use-package avro-mode
  :ensure nil
  :custom
  (tab-width 4)
  :mode "\\.avdl$")

;; ini files
(use-package conf-mode
  :ensure nil
  :mode "\\.ini\\'\\|\\.lock\\'\\|\\.service\\'\\|\\.desktop\\'")

;; csv files
(load-file (concat user-emacs-directory "mixins/languages/csv-config.el"))

;; Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package dockerfile-mode)

;; elm
(use-package elm-mode
  :hook (elm-mode . eglot-ensure))

;; go
(use-package go-mode
  :hook
  (go-mode . eglot-ensure)
  (go-mode . go-flymake)
  :ensure-system-package ("/usr/bin/gopls" . golang-x-tools-gopls))

(use-package go-flymake
  :init
  (setq flymake-allowed-file-name-masks nil)
  :ensure (go-flymake
           :host github
           :repo "dougm/goflymake"
           :files ("go-flymake.el")))

;; graphviz
(use-package graphviz-dot-mode
  :config (setq graphviz-dot-mode-indent-width 2))

;; handlebars
(use-package handlebars-mode)

;; Haskell
(load-file (concat user-emacs-directory "mixins/languages/haskell-config.el"))

;; JSON
(use-package json-mode
  :mode "\\.json\\'\\|\\.jshintrc\\'"
  :interpreter "json-mode"
  :hook (json-mode . eglot-ensure))

;; java
(use-package lsp-java
  :hook (java-mode . eglot-ensure))

;;; javascript & web
(load-file (concat user-emacs-directory "mixins/languages/web-config.el"))

;; Gherkin
(use-package pickle
  :mode "\\.feature\\'"
  :interpreter "pickle-mode")

;; Lisp
(use-package emacs-lisp-mode
  :ensure nil
  :mode "\\.el\\'"
  :hook
  (emacs-lisp-mode . flymake-mode)
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package lisp-mode
  :ensure nil
  :mode "\\.cl\\|\\.lisp\\'"
  :hook
  (lisp-mode . rainbow-delimiters-mode)
  (lisp-interaction-mode . rainbow-delimiters-mode))

(use-package slime
  :commands slime
  :custom
  (slime-kill-without-query-p t)
  (slime-repl-history-file (concat user-emacs-directory "slime-history.eld"))
  (slime-startup-animation nil)
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  :hook
  (slime-repl-mode . rainbow-delimiters-mode)
  ;; follow further instructions for installing quicklisp (package manager)
  ;; at https://github.com/susam/emacs4cl#get-started
  :ensure-system-package sbcl) ;; non-trivial installation

(use-package parinfer-rust-mode
  :custom
  (parinfer-rust-troublesome-modes ;; removed electric-pair-mode from list
   '(hungry-delete-mode global-hungry-delete-mode))
  (parinfer-rust-auto-download t)
  (electric-pair-local-mode -1)
  (parinfer-rust-preferred-mode 'paren)
  :hook
  (emacs-lisp-mode
   ielm-mode
   lisp-mode
   slime-repl-mode
   lisp-interaction-mode
   racket-mode)
  (before-save . delete-trailing-whitespace))

;; Markdown
(use-package markdown-mode
  :hook
  (markdown-mode . display-line-numbers-mode)
  (markdown-mode . auto-fill-mode)
  :mode "\\.md$\\|\\.markdown\\|\\.udon$"
  :interpreter "markdown-mode"
  :hook flyspell)

;; Python
(load-file (concat user-emacs-directory "mixins/languages/python-config.el"))

;; nix
(use-package nix-mode
  :requires magit-section
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  (if (featurep 'lsp-mode)
      (progn  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
              (lsp-register-client
               (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                                :major-modes '(nix-mode)
                                :server-id 'nix))))
  :hook (nix-mode . eglot-ensure)
  :mode "\\.nix\\'")

;; Racket
(use-package racket-mode
  :mode "\\.rkt\\'"
  :interpreter "racket-mode")

;; Rust
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :config
  (setq rustic-lsp-client 'eglot)
  (add-to-list
   'eglot-server-programs
   '((rust-ts-mode rust-mode)
     . ("rust-analyzer"
        :initializationOptions
        (:check (:command "clippy")))))
  :ensure-system-package
  (rust-analyzer . "rustup component add rust-analyzer"))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  (setq rustic-format-on-save t))

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
  :ensure (theta-mode
           :host github
           :repo "target/theta-idl"
           :branch "stage"
           :files ("emacs/theta-mode.el"))
  :mode ("\\.theta\\'" . theta-mode))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)
         ("user-data\\'" . yaml-mode)))

(use-package terraform-mode
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls")))
  :hook
  (terraform-mode . eglot-ensure)
  :ensure-system-package terraform-ls)

(use-package udev-mode)

;;; To use the hoon lsp server, build it from
;;; https://github.com/bct/hoon-language-server/tree/fix-issue-30
;;; and install it via [nvm](https://github.com/nvm-sh/nvm).

;;; Point to your planet with the right credentials, of course.

(use-package hoon-mode
  :ensure (hoon-mode
           :host github
           :protocol ssh
           :repo "dmvianna/hoon-mode.el"
           :branch "dev"
           :files (:defaults "*.json"))
  :custom
  (hoon-lsp-enable nil)
  (hoon-lsp-code "lidlut-tabwed-pillex-ridrup")
  (hoon-lsp-planet "zod")
  (hoon-lsp-port "8080")
  :bind (:map hoon-mode-map
              ("C-c r" . hoon-eval-region-in-herb)
              ("C-c b" . hoon-eval-buffer-in-herb))
  :init
  ;; lsp-mode
  (push '(hoon-mode . "hoon") lsp-language-id-configuration)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "hoon-language-server")
    :activation-fn (lsp-activate-on "hoon")
    :server-id 'hoon-language-server))
  :hook
  (hoon-mode . eldoc-box-hover-mode)
  (before-save . (lambda ()
                   (call-interactively
                    eglot-code-action-organize-imports))))

(use-package hoon-ts-mode
  :after combobulate
  :ensure (hoon-ts-mode
           :host github
           :repo "urbit-pilled/hoon-ts-mode")
  :custom
  (treesit-font-lock-level 4)
  :init
  (add-to-list 'combobulate-setup-functions-alist
               '(hoon . combobulate-hoon-setup)))

(use-package sql-indent
  :ensure (sql-indent
           :host github
           :repo "alex-hhh/emacs-sql-indent"
           :branch "master"))

(use-package bigquery-mode
  :ensure (bigquery-mode
           :host github
           :repo "dmvianna/bigquery-mode"
           :branch "quote"
           :files ("bigquery-mode.el" "bqm-names.el"))

  :ensure-system-package (gcloud . google-cloud-cli))

(provide 'languages)
;;; languages.el ends here.
