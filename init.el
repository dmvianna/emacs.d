;;; Package --- Summary
;;; Commentary:
;;; init file
;;; Code:

(require 'misc-config)
;; (use-package proxy-config)

;; git
(use-package magit
  :straight t
  :bind (:map
         magit-mode-map
         ("C-x g" . magit-status)))

;; Flycheck -- global syntax check (needed for hlint)
;; we need to upgrade the inbuilt flymake version so
;; that packages that require it don't fail
(use-package flymake
  :straight t)

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  )

(use-package flyspell
  :init
  (flyspell-mode t)
  :custom
  (ispell-program-name "hunspell")
  (ispell-local-dictionary "en_AU"))

(use-package langtool
  :straight t
  :custom
  (langtool-bin "languagetool-commandline")
  (langtool-default-language "en-AU"))

;; Company -- text completion
(use-package company
  :straight t)

(use-package docview
  :bind (:map
         docview-mode-map
         ("<mouse-4>" . doc-view-scroll-down-or-previous-page)
         ("<mouse-5>" . doc-view-scroll-up-or-next-page)
         ("<mouse-6>" . image-scroll-right)
         ("<mouse-7>" . image-scroll-left)
         ("<mouse-8>" . image-decrease-size)
         ("<mouse-9>" . image-increase-size)))

(use-package pdf-tools
  :straight t
  :custom (pdf-view-display-size 'fit-height)
  :init (pdf-tools-install))

(use-package image-mode
  :bind (:map
         image-mode-map
         ("<mouse-4>" . image-scroll-down)
         ("<mouse-5>" . image-scroll-up)
         ("<mouse-6>" . image-scroll-right)
         ("<mouse-7>" . image-scroll-left)
         ("<mouse-8>" . image-decrease-size)
         ("<mouse-9>" . image-increase-size)))

(use-package avro-mode
  :mode "\\.avdl$")

(use-package yasnippet
  :straight t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

;; LSP
(use-package lsp-mode
  :straight t
  :init (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom)
  :commands lsp-ui-mode)

(use-package company-lsp
  :straight t
  :after lsp-ui)

(use-package dockerfile-mode
  :straight t)

;; ini files
(use-package conf-mode
  :mode "\\.ini\\'\\|\\.lock\\'"
  )

;; graphviz
(use-package graphviz-dot-mode
  :straight t
  :config (setq graphviz-dot-mode-indent-width 2))
(use-package company-graphviz-dot)

;; JSON
(use-package json-mode
  :straight t
  :mode "\\.json\\'\\|\\.jshintrc\\'"
  :interpreter "json-mode"
  )

;; Gherkin
(use-package pickle
  :straight t
  :mode "\\.feature\\'"
  :interpreter "pickle-mode")

;; Haskell

(use-package haskell-mode
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
  :straight t
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  (lsp-haskell-tactic-on t)
  (lsp-haskell-completion-snippets-on t)
  (lsp-haskell-format-on-import-on t)
  (lsp-haskell-formatting-provider "fourmolu")
  (lsp-haskell-fourmolu-on t)
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp)
  )

;; Dhall
(use-package dhall-mode
  :straight t
  :mode "\\.dhall\\'")

;; Lisp
(use-package paredit
  :straight t
  :hook
  ((emacs-lisp-mode . paredit-mode)
    (lisp-mode . paredit-mode)
    (lisp-interaction-mode . paredit-mode)
    (geiser-mode . paredit-mode)
    (racket-mode . paredit-mode)))

;; Markdown
(use-package markdown-mode
  :straight t
  :config
  :mode "\\.md$"
  :interpreter "markdown-mode"
  :hook flyspell)

;; Python

(use-package py-isort
  :straight t
  :after python
  :hook (before-save . py-isort-before-save))

(use-package python-black
  :straight t
  :hook (python-mode . python-black))

(use-package lsp-jedi
  :straight t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    ;; (add-to-list 'lsp-enabled-clients 'jedi)
    )
  :hook (python-mode . (lambda ()
                         (require 'lsp-jedi)
                         (lsp)
                         (flycheck-add-next-checker
                          'lsp
                          'python-pyright)
                         (flycheck-add-next-checker
                          'python-pyright
                          'python-pylint))))

;; Racket
(use-package racket-mode
  :straight t
  :mode "\\.rkt\\'"
  :interpreter "racket-mode")

;; rainbow
;; rainbow-delimiters for elisp
(use-package rainbow-delimiters
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
  :custom (sh-basic-offset 2)
  )

(provide 'init)
;;; init ends here
