;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Scala configuration
;;; Code:

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode "\\.sc\\'\\|\\.scala\\'"
  :hook
  (scala-mode . envrc-mode)
  (scala-mode . eglot-ensure)
  ;; :init
  ;; (setq-local eglot-workspace-configuration
  ;;             '((metals (scalafmt-config-path (".scalafmt.conf"))))
  ;;             )
  )

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
  (setq sbt:program-options
        '("-Dsbt.supershell=false"
          "-J-XX:+UseParallelGC" ; improve GC performance by leveraging multiple processors
          "-J-Xmx2g" ; maximum size of the memory allocation pool
          "-J-Xms100m" ; minimum and initial size of the heap
          "-J-Xss4m")) ; thread stack size
  :hook (sbt-mode . fix-sbt-movement))

(use-package lsp-metals
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")))

(provide 'scala-config)
;;; scala-config.el ends here
