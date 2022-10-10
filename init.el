;;; Package --- Summary
;;; Commentary:
;;;
;;; Reload this config with M-: (load user-init-file)
;;;
;;; init file
;;; Code:

(require 'misc-config)
;; (use-package proxy-config)

;;; stuff that is required to make emacs usable

;; git
(use-package magit
  :straight t
  :init
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil))
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
  (setq-default flycheck-temp-prefix ".flycheck"))


;; Company -- text completion
(use-package company
  :straight t)

;;; Check spelling -- I don't even know how this works

;; (use-package flyspell
;;   :init
;;   (flyspell-mode t)
;;   :custom
;;   (ispell-program-name "hunspell")
;;   (ispell-local-dictionary "en_AU"))

;; (use-package langtool
;;   :straight t
;;   :custom
;;   (langtool-bin "languagetool-commandline")
;;   (langtool-default-language "en-AU"))

;;; viewers

(use-package docview
  :straight nil
  :bind (:map
         docview-mode-map
         ("<mouse-4>" . doc-view-scroll-down-or-previous-page)
         ("<mouse-5>" . doc-view-scroll-up-or-next-page)
         ("<mouse-6>" . image-scroll-right)
         ("<mouse-7>" . image-scroll-left)
         ("<mouse-8>" . image-decrease-size)
         ("<mouse-9>" . image-increase-size)))

(use-package pdf-tools
  :custom (pdf-view-display-size 'fit-height)
  :init
  (pdf-tools-install)
  :hook
  (pdf-view-mode . (lambda (display-line-numbers-mode nil))))

(use-package image-mode
  :straight nil
  :bind (:map
         image-mode-map
         ("<mouse-4>" . image-scroll-down)
         ("<mouse-5>" . image-scroll-up)
         ("<mouse-6>" . image-scroll-right)
         ("<mouse-7>" . image-scroll-left)
         ("<mouse-8>" . image-decrease-size)
         ("<mouse-9>" . image-increase-size)))

;;; helpers

(use-package which-key
  :config
  (which-key-mode))

(use-package google-this)

;;
;; Re-spawn scratch buffer when killed
;;
(use-package immortal-scratch
  :init
  (setq initial-scratch-message "")
  (setq initial-major-mode 'text-mode)
  :hook
  (after-init . immortal-scratch-mode))

(use-package keychain-environment
  :init (keychain-refresh-environment))

(use-package yasnippet)

;;; LSP
(use-package lsp-mode
  ;; lsp does not define this variable by
  ;; default, so we have to set it here
  :custom (lsp-enable-snippet nil)
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom)
  :commands lsp-ui-mode)

(use-package company-lsp
  :after lsp-ui)

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
(require 'haskell-config)

;; JSON
(use-package json-mode
  :mode "\\.json\\'\\|\\.jshintrc\\'"
  :interpreter "json-mode"
  )

;;; javascript & web
(require 'web-config)

;; Gherkin
(use-package pickle
  :mode "\\.feature\\'"
  :interpreter "pickle-mode")

;; Lisp
(use-package parinfer
  :hook
  ((emacs-lisp-mode . parinfer-mode)
   (lisp-mode . parinfer-mode)
   (lisp-interaction-mode . parinfer-mode)
   (geiser-mode . parinfer-mode)
   (racket-mode . parinfer-mode)))

;; Markdown
(use-package markdown-mode
  :hook
  (markdown-mode . display-line-numbers-mode)
  :mode "\\.md$"
  :interpreter "markdown-mode"
  :hook flyspell)

;; Python
(require 'python-config)

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

;; rainbow
;; rainbow-delimiters for elisp
(use-package rainbow-delimiters
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

(use-package theta-mode
  :straight (theta-mode
             :type git
             :host github
             :repo "target/theta-idl"
             :branch "stage"
             :files ("emacs/theta-mode.el"))
  :mode ("\\.theta\\'" . theta-mode))

(use-package yaml-mode)

;;; shells

;; rest
(use-package verb)

;; everything
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind
  (:map org-mode-map
        ("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; term
(use-package multi-term)

;; elisp term
(use-package aweshell
  :straight (aweshell
             :type git
             :host github
             :repo "manateelazycat/aweshell"
             :files ("aweshell.el"
                     "eshell-did-you-mean.el"
                     "eshell-prompt-extras.el"
                     "eshell-up.el"
                     "exec-path-from-shell.el")))

(use-package google-this)

;;
;; Re-spawn scratch buffer when killed
;;
(use-package immortal-scratch
  :init
  (setq initial-scratch-message "")
  (setq initial-major-mode 'text-mode)
  :hook
  (after-init . immortal-scratch-mode))

;; ssh-add
(use-package keychain-environment
  :init (keychain-refresh-environment))

(use-package string-inflection)

(use-package visual-regexp)

(use-package visual-regexp-steroids
  :straight (visual-regexp-steroids
             :type git
             :host github
             :repo "benma/visual-regexp-steroids.el"
             :files ("visual-regexp-steroids.el" "regexp.py"))
  :requires visual-regexp
  :bind (:map global-map
              ("C-c r" . vr/replace)
              ("C-c q" . vr/query-replace)
              ("C-r" . vr/isearch-backward)
              ("C-s" . vr/isearch-forward)))


(use-package multiple-cursors
  :bind (:map global-map
              ("C-c m" . vr/mc-mark)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
              ("C-c C-<" . mc/mark-all-like-this)))

(use-package moldable-emacs
  :straight (moldable-emacs
             :type git
             :host github
             :repo "ag91/moldable-emacs"
             :files ("moldable-emacs.el"))
  :bind (:map global-map
              ("C-c n m" . me-mold)
              ("C-c n f" . me-go-forward)
              ("C-c n b" . me-go-back)
              ("C-c n o" . me-open-at-point)
              ("C-c n d" . me-mold-docs)
              ("C-c n g" . me-goto-mold-source)
              ("C-c n e a" . me-mold-add-last-example))
  :config
  (me-setup-molds))

(provide 'init)
;;; init.el ends here
