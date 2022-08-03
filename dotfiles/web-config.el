;;; Package --- Summary
;;; Commentary:
;;; All things web
;;; Code:

(use-package js-mode
  :hook
  ((js-mode . lsp)
   (js-mode . (lambda ()
                (require 'tide)
                (tide-setup)))))

(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ;; ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-comment-style 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-comment-keywords t)
  (web-mode-enable-current-element-highlight t)
  :hook
  ((web-mode . (lambda ()
                  (require 'tide)
                  (tide-setup)))
   (web-mode . lsp)))

(use-package prettier-js
  :straight t
  :commands (prettier-js-mode prettier)
  :custom
  (prettier-target-mode "js-mode")
  (prettier-js-args
   '("--trailing-comma" "all" "--single-quote" "--semi" "--arrow-parens" "always"))
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package typescript-mode
  :straight t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :custom
  (typescript-indent-level 2)
  :hook
  ((typescript-mode . subword-mode)
   (typescript-mode . lsp)
   (typescript-mode . (lambda ()
                        (require 'tide)
                        (tide-setup))))
  :mode
  ("\\.tsx?\\'" . typescript-tsx-mode))

(use-package tree-sitter
  :straight t
  :hook
  ((typescript-mode . tree-sitter-hl-mode)
   (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package tide
  :straight t
  :commands tide-setup
  :custom
  (typescript-indent-level 2)
  (tide-format-options '(:indentSize 2 :tabSize 2)))

(provide 'web-config)
;;; web-config.el ends here
