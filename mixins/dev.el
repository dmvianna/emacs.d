;;; dev.el --- Summary
;;; Commentary:
;;; Cross language functionality.
;;; Code:

;; rainbow-delimiters -- excellent for any language,
;; critical for lisps
(use-package rainbow-delimiters
 :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; we need to upgrade the inbuilt flymake version so
;; that packages that require it don't fail
(use-package flymake
:bind (:map flymake-mode-map (("M-n" . flymake-goto-next-error)
                              ("M-p" . flymake-goto-prev-error))))

;; Syntax checking and everything else related to it
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  :custom
  (flycheck-disabled-checkers '(haskell-stack-ghc haskell-ghc haskell-hlint)))

;; Company -- text completion
(use-package company
  :delight
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1)
  (global-company-mode t))

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
  :straight (undo-hl :type git :host github :repo "casouri/undo-hl")
  :hook ((text-mode . undo-hl-mode)
         (prog-mode . undo-hl-mode))
  :custom-face
  (undo-hl-insert ((t (:background "#FFFF00"))))
  (undo-hl-delete ((t (:background "#FFFF00")))))

(use-package vundo
  :after undo-hl
  :straight (vundo :type git :host github :repo "casouri/vundo")
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
 :ensure-system-package (rg . ripgrep))

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

;; org-mode
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

;; git shell ?! :-DDDD
(use-package magit
  :init
  ;; status is expensive in big repos, only refresh if
  ;; it is the current buffer
  (setq magit-refresh-status-buffer nil)
  ;; it is always git, so no need to display it
  ;; https://emacs.stackexchange.com/a/10957/3895
  (defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))
  (setq vc-display-status nil) ;; don't display branch name in mode line
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil))
  :bind (:map
         magit-mode-map
         ("C-x g" . magit-status)))

(use-package abridge-diff
  :after magit ;; optional, if you'd like to use with magit
  :init (abridge-diff-mode 1))

;; the current transient release (magit dependency)
;; is broken (v0.3.7), so we use this
(use-package transient
  :straight (transient
             :type git
             :host github
             :repo "magit/transient"))


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
