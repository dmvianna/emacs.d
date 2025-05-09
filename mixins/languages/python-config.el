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

(use-package py-isort
  :after python
  :hook (before-save . py-isort-before-save)
  :ensure-system-package (isort . python3-isort))

(use-package lazy-ruff
  :after python
  :bind (("C-c f" . lazy-ruff-lint-format-dwim))
  :ensure-system-package (ruff))

(use-package python-ts-mode
  :mode ("\\.py\\'" . python-ts-mode)
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((python-mode . "pyright")))
  (eglot-workspace-configuration . "{
    \"python.analysis.typeChecking\" : true,
    \"python.analysis.typeCheckingMode\" : \"strict\",
    \"python.analysis.autoImportCompletions\" : true,
    \"python.analysis.extraPaths\": [
      \"/path/to/your/project/src\"
    ]
  }")
  :hook
  (python-ts-mode . eglot-ensure)
  :ensure-system-package (pyright-langserver . "uv tool install pyright"))

(provide 'python-config)
;;; python-config.el ends here
