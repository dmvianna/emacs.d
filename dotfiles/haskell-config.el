;;; Package --- Summary
;;; Commentary:
;;; Haskell configuration
;;; Code:

(use-package haskell-mode
  :straight t
  :config
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  ;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  ;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
  :custom
  (haskell-indentation-ifte-offset 4)
  (haskell-indentation-layout-offset 4)
  (haskell-indentation-left-offset 4)
  (haskell-indentation-starter-offset 4)
  (haskell-indentation-where-post-offset 4)
  (haskell-indentation-where-pre-offset 4))

(use-package haskell-interactive-mode
  :straight nil
  :hook
  (haskell-mode-hook . interactive-haskell-mode))

(use-package haskell-process
  :straight nil
  :custom
  (haskell-process-type 'stack-ghci)
  (haskell-indent-spaces 4))

(use-package lsp-haskell
  :straight t
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
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp))

(provide 'haskell-config)
;;; haskell-config.el ends here
