;;; early-modes.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Modes that need to be loaded early, before language modes and such,
;;; to work properly.
;;; Code:

;; direnv
;; (use-package direnv
;;     :config
;;     ;; enable globally
;;     (direnv-mode)
;;     ;; exceptions
;;     ;; (add-to-list 'direnv-non-file-modes 'foobar-mode)

;;     ;; nix-shells make too much spam -- hide
;;     (setq direnv-always-show-summary nil)

;;     :hook
;;     ;; ensure direnv updates before flycheck and lsp
;;     ;; https://github.com/wbolster/emacs-direnv/issues/17
;;     (flycheck-before-syntax-check . direnv-update-environment)
;;     (lsp-before-open-hook . direnv-update-environment)

;;     :custom
;;     ;; quieten logging
;;     (warning-suppress-types '((direnv))))

(use-package inheritenv
  :elpaca (inheritenv :host github :repo "purcell/inheritenv"))

(use-package envrc
  :delight " env"
  :elpaca (envrc :host github :repo "purcell/envrc")
  :commands (envrc-mode)
  :init (envrc-global-mode))

(use-package project
  :elpaca nil
  :custom (project-vc-extra-root-markers '(".envrc")))

(provide 'early-modes)
;;; early-modes.el ends here
