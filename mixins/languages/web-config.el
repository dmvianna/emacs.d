;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; All things web
;;; Code:

(use-package js-ts-mode
  :ensure nil
  :mode (("\\.c?js\\'" . js-ts-mode))
  :hook
  ((js-mode . lsp)
   (js-mode . (lambda ()
                (require 'tide)
                (tide-setup)))))

(use-package js-jsx-mode
  :ensure nil
  :mode (("\\.jsx\\'" . js-jsx-mode))
  :hook (js-jsx-mode . (lambda ()
                         (require 'tide)
                         (tide-setup))))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
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
  (web-mode-enable-current-element-highlight t))

(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :custom
  (typescript-indent-level 2)
  :hook
  ((typescript-mode . subword-mode)
   (typescript-mode . (lambda ()
                        (require 'tide)
                        (tide-setup))))
  :mode
  ("\\.tsx\\'" . typescript-tsx-mode)
  ("\\.m?ts\\'" . typescript-mode))

(use-package tree-sitter
  :hook
  ((js-ts-mode . tree-sitter-hl-mode)
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
