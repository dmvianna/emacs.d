;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Python configuration
;;; Code:

;; Goes into infinite loop when used with LSP
;; (use-package poetry
;;   :hook
;;   (python-mode . poetry-tracking-mode))

(use-package py-isort
  :after python
  :hook (before-save . py-isort-before-save))

(use-package python-black
  :after python
  :hook (python-ts-mode . python-black-on-save-mode))

;; (use-package lsp-jedi
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls))
;;   ;; (add-to-list 'lsp-enabled-clients 'jedi)
;;   (require 'lsp-diagnostics)
;;   (lsp-diagnostics-flycheck-enable)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-jedi)
;;                          (lsp)
;;                          (poetry-tracking-mode)
;;                          (flycheck-add-next-checker
;;                           'lsp
;;                           'python-pyright)
;;                          (flycheck-add-next-checker
;;                           'python-pyright
;;                           'python-pylint))))

(provide 'python-config)
;;; python-config.el ends here
