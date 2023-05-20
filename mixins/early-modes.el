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

;; git shell ?! :-DDDD
(use-package magit
  :elpaca nil
  :init
  ;; status is expensive in big repos, only refresh if
  ;; it is the current buffer
  ;; (setq magit-refresh-status-buffer nil)
  ;; it is always git, so no need to display it
  ;; https://emacs.stackexchange.com/a/10957/3895
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     " " vc-mode)))
        (setq vc-mode noback))))
  (setq vc-display-status nil) ;; don't display branch name in mode line
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil))
  :bind (:map
         magit-mode-map
         ("C-x g" . magit-status)))

(use-package inheritenv
  :elpaca (inheritenv :host github :repo "purcell/inheritenv"))

(use-package envrc
  :delight " env"
  :elpaca (envrc :host github :repo "purcell/envrc")
  :commands (envrc-mode)
  :init (envrc-global-mode))

(use-package project
  :init
  (setq project-vc-extra-root-markers '(".envrc")))

(provide 'early-modes)
;;; early-modes.el ends here
