;;; dev.el --- Summary
;;; Commentary:
;;; Cross language functionality.
;;; Code:

;; rainbow-delimiters -- excellent for any language,
;; critical for lisps
(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode geiser-mode))

;; we need to upgrade the inbuilt flymake version so
;; that packages that require it don't fail
(use-package flymake)

;; Syntax checking and everything else related to it
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-executable-find
        (lambda
          (cmd)
          (direnv-update-environment default-directory)
          (executable-find cmd)))
  (setq-default flycheck-temp-prefix ".flycheck"))


;; Company -- text completion
(use-package company
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

;;; format code helper
(use-package format-all)

(use-package yasnippet)

;; code folding, very useful with big JSON files
(use-package origami
  :bind (:map origami-mode-map ("TAB" . origami-toggle-node)))

;; use tab and let lisp figure out the closing parens...
(use-package smartparens
  :config (smartparens-global-mode t))

;;; save lots of undo history
;;; manual: https://www.dr-qubit.org/undo-tree/undo-tree.txt
;;; memory management: https://www.dr-qubit.org/Lost_undo-tree_history.html
(use-package undo-tree
  :custom
  (undo-tree-save-history nil)
  (undo-tree-load-history nil)
  (undo-tree-limit 8000000) ;; 10 times smaller than default
  (undo-tree-strong-limit 12000000) ;; 10 times smaller than default
  (undo-tree-outer-limit 360000000) ;; 10 times smaller than default
  :init
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; shells
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; because in Big Sur this is set to /
(setq default-directory (getenv "HOME"))

;; direnv
(use-package direnv
  :config
  ;; enable globally
  (direnv-mode)
  ;; exceptions
  ;; (add-to-list 'direnv-non-file-modes 'foobar-mode)

  ;; nix-shells make too much spam -- hide
  (setq direnv-always-show-summary nil)

  :hook
  ;; ensure direnv updates before flycheck and lsp
  ;; https://github.com/wbolster/emacs-direnv/issues/17
  (flycheck-before-syntax-check . direnv-update-environment)
  (lsp-before-open-hook . direnv-update-environment)

  :custom
  ;; quieten logging
  (warning-suppress-types '((direnv))))

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
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil))
  :bind (:map
         magit-mode-map
         ("C-x g" . magit-status)))

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
