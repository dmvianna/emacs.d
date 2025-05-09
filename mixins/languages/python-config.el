;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Python configuration
;;; Code:

(use-package poetry
  :ensure-system-package (poetry))

(use-package uv-mode
  :delight " UV"
  :hook (python-ts-mode . uv-mode-auto-activate-hook)
  :ensure-system-package (uv))

(use-package python-isort
  :after python
  :hook
  (python . python-isort-on-save-mode)
  (eglot-managed-mode . python-isort-on-save-mode)
  :ensure-system-package (isort . python3-isort))

(use-package lazy-ruff
  :after python
  :bind (("C-c f" . lazy-ruff-lint-format-dwim))
  :ensure-system-package (ruff))

(use-package flymake-ruff
  :ensure (flymake-ruff
           :type git
           :host github
           :repo "erickgnavar/flymake-ruff")
  ;; :hook (eglot-managed-mode . flymake-ruff-load)
  :ensure-system-package (ruff))

(use-package python-ts-mode
  :mode ("\\.py\\'" . python-ts-mode)
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((python-mode . "pyright")))
  (eglot-workspace-configuration
   . (:python
      (:analysis (:typeChecking t
                  :typeCheckingMode "strict"
                  :autoImportCompletions t
                  :extraPaths '()))))
  :hook
  (python . eglot-ensure)
  :ensure-system-package (pyright-langserver . "uv tool install pyright"))

(provide 'python-config)
;;; python-config.el ends here
