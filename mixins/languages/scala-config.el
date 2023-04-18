;;; Package --- Summary
;;; Commentary:
;;; Scala configuration
;;; Code:

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :hook (scala-mode . envrc-mode)
  :hook (scala-mode . eglot-ensure))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (defun fix-sbt-movement ()
    (local-set-key (kbd "C-p") 'comint-previous-input)
    (local-set-key (kbd "C-n") 'comint-next-input))
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :hook (sbt-mode . fix-sbt-movement))

(use-package lsp-metals
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")))

(provide 'scala-config)
;;; scala-config.el ends here
