;;; Package --- Parents  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Packages that must be present in the system before other packages
;;; are downloaded.
;;; Code:

;; git shell ?! :-DDDD

;; nix-mode depends on a package that comes with magit, so we fetch magit
;; before moving forward. Below we tell elpaca to not continue until the
;; current download queue is empty.

(use-package magit
  :init
  ;; status is expensive in big repos, only refresh if
  ;; it is the current buffer
  ;; (setq magit-refresh-status-buffer nil)
  ;; it is always git, so no need to display it
  ;; https://emacs.stackexchange.com/a/10957/3895
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     " " vc-mode)))
        (setq vc-mode noback))))
  (setq vc-display-status nil) ;; don't display branch name in mode line
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil))
  :ensure-system-package (magit . git)
  :bind (:map
         magit-mode-map
         ("C-x g" . magit-status)))

;;; LSP

(use-package lsp-mode
  ;; lsp does not define this variable by
  ;; default, so we have to set it here
  :custom
  (lsp-enable-snippet nil)
  :init
  (setq
   ;; easier than debugging right now https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
   lsp-enable-file-watchers nil
   ;; give lsp enough memory
   read-process-output-max (* 1024 1024) ;; 1mb)
   ;; Probably overkill, but doesn't hurt.
   ;; LSP should use better deserialisation.
   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
   lsp-use-plists t
   ;; Yes, I want to restart
   lsp-restart 'auto-restart
   lsp-enable-folding nil
   lsp-enable-snippet nil)
  :hook
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)
  :commands (lsp lsp-deferred)
  :bind-keymap ("s-l" . lsp-command-map))

;; wait until these packages are downloaded before continuing.
(elpaca-wait)

(provide 'parents.el)
;;; parents.el ends here
