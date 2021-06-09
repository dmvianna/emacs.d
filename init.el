;;; Package --- Summary
;;; Commentary:
;;; init file
;;; Code:

(require 'misc-config)
;; (use-package proxy-config)

;; Markdown
(use-package
  markdown-mode
  :ensure t
  :straight t
  :config
  :mode "\\.md$"
  :interpreter "markdown-mode"
  )

;; Flycheck -- global syntax check (needed for hlint)
(use-package
  flycheck
  :ensure t
  :straight t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  )

;; Company -- text completion
(use-package company
  :ensure t
  :straight t)
