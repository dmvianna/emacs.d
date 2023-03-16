;;; Package --- Summary
;;; Commentary:
;;; Haskell configuration.
;;; Code:

(use-package haskell-process
  :straight nil
  :custom
  (haskell-process-type 'auto))

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
  (lsp-haskell-stylish-haskell nil))

(use-package haskell-cabal
  :straight nil
  :hook
  (haskell-cabal-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

(provide 'haskell-config)
;;; haskell-config.el ends here
