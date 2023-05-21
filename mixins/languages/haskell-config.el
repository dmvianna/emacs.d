;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Haskell configuration.
;;; Code:

(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  (lsp-haskell-tactic-on t)
  (lsp-haskell-completion-snippets-on t)
  (lsp-haskell-format-on-import-on t)
  (lsp-haskell-formatting-provider "fourmolu")
  (lsp-haskell-fourmolu-on t)
  (lsp-haskell-brittany nil)
  (lsp-haskell-floskell nil)
  (lsp-haskell-ormolu nil)
  (lsp-haskell-stylish-haskell nil)
  :init
  (setq-default eglot-workspace-configuration
                '(haskell
                  (formattingProvider "fourmolu"))))

(use-package haskell-cabal
  :elpaca nil
  :hook
  (haskell-mode . envrc-mode)
  (haskell-cabal-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

(use-package haskell-mode
  :elpaca nil
  :delight "λ"
  :after haskell-font-lock
  :hook (haskell-mode . eglot-ensure))

(provide 'haskell-config)
;;; haskell-config.el ends here.
