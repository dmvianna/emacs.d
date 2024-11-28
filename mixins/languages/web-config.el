;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; All things web
;;; Code:

(use-package js-mode
  :ensure nil
  :hook
  ((js-mode . lsp)
   (js-mode . (lambda ()
                (require 'tide)
                (tide-setup)))))

(use-package web-mode
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
   (web-mode . eglot)))

(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  (put 'typescript-tsx-mode 'eglot-language-id "typescriptreact")
  :custom
  (typescript-indent-level 2)
  :hook
  ((typescript-tsx-mode . subword-mode)
   (typescript-tsx-mode . (lambda ()
                            (require 'tide)
                            (tide-setup))))
  :mode
  ("\\.tsx?\\'" . typescript-tsx-mode))

(use-package tree-sitter
  :hook
  ((typescript-mode . tree-sitter-hl-mode)
   (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package tide
  :commands tide-setup
  :custom
  (typescript-indent-level 2)
  (tide-format-options '(:indentSize 2 :tabSize 2)))

(provide 'web-config)
;;; web-config.el ends here
