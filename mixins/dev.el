;;; dev.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Cross language functionality.
;;; Code:

;; rainbow-delimiters -- excellent for any language,
;; critical for lisps
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; helps reading yaml
(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character))

;; we need to upgrade the inbuilt flymake version so
;; that packages that require it don't fail
(use-package flymake
  :bind (:map flymake-mode-map (("M-n" . flymake-goto-next-error)
                                ("M-p" . flymake-goto-prev-error))))

;; Syntax checking and everything else related to it
(use-package flycheck
  ;; :init (global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  :custom
  (flycheck-disabled-checkers '(haskell-stack-ghc haskell-ghc haskell-hlint)))

(use-package aggressive-indent)

;;; format code helper
(use-package format-all
  :config
  (custom-set-variables '(format-all-formatters '(("Haskell" fourmolu))))
  :hook (format-all-mode . format-all-ensure-formatter))

(use-package yasnippet)

;; code folding, very useful with big JSON files
(use-package origami
  :bind (:map origami-mode-map ("TAB" . origami-toggle-node)))

;; use tab and let lisp figure out the closing parens...
(use-package smartparens
  :delight
  :config (smartparens-global-mode t))

;;; save lots of undo history
;;; manual: https://www.dr-qubit.org/undo-tree/undo-tree.txt
;;; memory management: https://www.dr-qubit.org/Lost_undo-tree_history.html
;; (use-package undo-tree
;;   :custom
;;   (undo-tree-auto-save-history nil)
;;   (undo-tree-limit 8000000) ;; 10 times smaller than default
;;   (undo-tree-strong-limit 12000000) ;; 10 times smaller than default
;;   (undo-tree-outer-limit 360000000) ;; 10 times smaller than default
;;   :init
;;   (setq undo-tree-enable-undo-in-region nil)
;;   (defadvice undo-tree-make-history-save-file-name
;;       (after undo-tree activate)
;;     (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package undo-hl
  :delight
  :elpaca (undo-hl :host github :repo "casouri/undo-hl")
  :hook ((text-mode . undo-hl-mode)
         (prog-mode . undo-hl-mode))
  :custom-face
  (undo-hl-insert ((t (:background "#FFFF00"))))
  (undo-hl-delete ((t (:background "#FFFF00")))))

(use-package vundo
  :after undo-hl
  :elpaca (vundo :host github :repo "casouri/vundo")
  :config
  ;; this is all so that undo-hl works with vundo. It mostly does.
  (setq undo-hl-undo-commands
        (-union
         undo-hl-undo-commands
         '(vundo-backward vundo-forward vundo-stem-root vundo-stem-end)))
  (defvar my-real-vundo-buf nil)
  (defun my-vundo-forward-pre-command-hook ()
    (let ((buf (current-buffer)))
      (when (bound-and-true-p my-real-vundo-buf)
        (unless (eq buf my-real-vundo-buf)
          (with-current-buffer my-real-vundo-buf (run-hooks 'pre-command-hook))))))
  ;; also undo-hl stuff
  (add-hook 'vundo-pre-enter-hook
            (lambda ()
              (setq my-real-vundo-buf (current-buffer))
              (add-hook 'pre-command-hook 'my-vundo-forward-pre-command-hook)
              (undo-hl-mode 1)))
  (add-hook 'vundo-post-exit-hook
            (lambda ()
              (remove-hook 'pre-command-hook 'my-vundo-forward-pre-command-hook)
              (undo-hl-mode -1)
              (makunbound 'my-real-vundo-buf)))
  ;; Better contrasting highlight.
  :custom-face
  (vundo-node ((t (:foreground "#808080"))))
  (vundo-stem ((t (:foreground "#808080"))))
  (vundo-highlight ((t (:foreground "#FFFF00")))))

;;; exclude the following directories from grep searches
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "straight")
     (add-to-list 'grep-find-ignored-directories "elpaca")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "amazonka")
     (add-to-list 'grep-find-ignored-directories "amazonka-1.6.1")
     (add-to-list 'grep-find-ignored-directories "amazonka-2.0.0")
     (add-to-list 'grep-find-ignored-directories ".stack-work")))
(setq wgrep-enable-key (kbd "C-c C-c"))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; file search
(use-package rg
  :ensure-system-package (rg . ripgrep))


;; fallback cross file definition lookup
(use-package dumb-jump
  :ensure-system-package (dumb-jump . ripgrep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; shells
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; because in Big Sur this is set to /
(setq default-directory (getenv "HOME"))

;; exec path from shell
(use-package exec-path-from-shell
  :config
  (dolist (var '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                 "SSH_AGENT_PID" "SSH_AUTH_SOCK"
                 "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "XAUTHORITY"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; ssh-add
(use-package keychain-environment
  :init (keychain-refresh-environment))


;; rest
(use-package verb)

;; org-related
(use-package org-pandoc-import
  :elpaca (:host github
                 :repo "tecosaur/org-pandoc-import"
                 :files ("*.el" "filters" "preprocessors")))

(use-package org-roam)

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Documents/Notes/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank)))
  :custom (org-download-method 'attach)
  :ensure-system-package (org-download . wl-clipboard))

;; term
(use-package multi-term)

;; elisp term
(use-package aweshell
  :elpaca (aweshell
           :host github
           :repo "manateelazycat/aweshell"
           :files ("aweshell.el"
                   "eshell-did-you-mean.el"
                   "eshell-prompt-extras.el"
                   "eshell-up.el"
                   "exec-path-from-shell.el")))


(use-package abridge-diff
  :after magit)

;; the current transient release (magit dependency)
;; is broken (v0.3.7), so we use this
(use-package transient
  :elpaca (transient
           :host github
           :repo "magit/transient"))

(use-package ediff
  :elpaca nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-prefer-iconified-control-frame t))

;; major mode for viewing logs
(use-package logview)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   web browser
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; open file in browser
(global-set-key "\C-c\C-zv" 'browse-url-of-file)

;; set browser
(setq browse-url-browser-function 'browse-url-chrome)

;; google stuff
(use-package google-this)

(provide 'dev)
;;; dev.el ends here
