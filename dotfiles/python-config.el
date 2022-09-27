;;; Package --- Summary
;;; Commentary:
;;; Python configuration
;;; Code:

(use-package py-isort
  :after python
  :hook (before-save . py-isort-before-save))

(use-package python-black
  :hook (python-mode . python-black))

(use-package lsp-jedi
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls))
  ;; (add-to-list 'lsp-enabled-clients 'jedi)
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  :hook (python-mode . (lambda ()
                         (require 'lsp-jedi)
                         (lsp)
                         (flycheck-add-next-checker
                          'lsp
                          'python-pyright)
                         (flycheck-add-next-checker
                          'python-pyright
                          'python-pylint))))

(provide 'python-config)
;;; python-config.el ends here
